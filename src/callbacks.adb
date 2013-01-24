--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with System;

with asm_generic_int_ll64_h;
with xfrm_h;

with Tkmrpc.Types;
with Tkmrpc.Results;
with Tkmrpc.Clients.Ees;

with Xfrm.Thin;

with Logger;

package body Callbacks
is

   package L renames Logger;

   procedure Process_Acquire (Message : access Xfrm.Thin.Nlmsghdr_Type);
   --  Process Kernel ACQUIRE message.

   procedure Process_Expire (Message : access Xfrm.Thin.Nlmsghdr_Type);
   --  Process Kernel EXPIRE message.

   -------------------------------------------------------------------------

   procedure Handle_Message
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Netlink.Netlink_Addr_Type)
   is
      Msg : aliased Xfrm.Thin.Nlmsghdr_Type;

      for Msg'Address use Item'Address;

      Action : Xfrm.Thin.Xfrm_Msg_Type;
   begin
      if not Xfrm.Thin.Nlmsg_Ok (Msg => Msg,
                                 Len => Item'Length)
      then
         L.Log (Message => "Ignoring invalid Netlink message from pid"
                & Src'Img);
         return;
      end if;

      Action := Xfrm.Thin.Xfrm_Msg_Type'Enum_Val (Msg.Nlmsg_Type);
      L.Log (Message => "Netlink message received (" & Item'Length'Img
             & " bytes ) from pid" & Src'Img & ", type " & Action'Img);

      case Action is
         when Xfrm.Thin.XFRM_MSG_ACQUIRE =>
            Process_Acquire (Message => Msg'Access);
         when Xfrm.Thin.XFRM_MSG_EXPIRE =>
            Process_Expire (Message => Msg'Access);
         when others => null;
      end case;
   end Handle_Message;

   -------------------------------------------------------------------------

   procedure Process_Acquire (Message : access Xfrm.Thin.Nlmsghdr_Type)
   is
      Addr     : System.Address
        := Xfrm.Thin.Nlmsg_Data
          (Msg => Message,
           Len => xfrm_h.xfrm_user_acquire'Size / 8);
      Rta_Size : Natural
        := Xfrm.Thin.Nlmsg_Payload
          (Msg => Message.all,
           Len => xfrm_h.xfrm_user_acquire'Size / 8);
   begin
      loop
         declare
            Attr_Kind : xfrm_h.xfrm_attr_type_t;
            Rta       : aliased Xfrm.Thin.Rtattr_Type;
            for Rta'Address use Addr;
            pragma Import (Ada, Rta);
         begin
            exit when not Xfrm.Thin.Rta_Ok
              (Rta => Rta,
               Len => Rta_Size);

            Attr_Kind := xfrm_h.xfrm_attr_type_t'Val
              (Rta.Rta_Type);
            case Attr_Kind is
               when xfrm_h.XFRMA_TMPL =>
                  declare
                     use type Tkmrpc.Results.Result_Type;

                     Status    : Tkmrpc.Results.Result_Type;
                     Tmpl_Addr : constant System.Address
                       := Xfrm.Thin.Rta_Data (Rta => Rta'Access);
                     Tmpl      : xfrm_h.xfrm_user_tmpl;
                     for Tmpl'Address use Tmpl_Addr;
                     pragma Import (Ada, Tmpl);
                  begin
                     L.Log (Message => "Initiating ESA acquire for reqid"
                            & Tmpl.reqid'Img);
                     Tkmrpc.Clients.Ees.Esa_Acquire
                       (Result => Status,
                        Sp_Id  => Tkmrpc.Types.Sp_Id_Type (Tmpl.reqid));
                     if Status /= Tkmrpc.Results.Ok then
                        L.Log (Level   => L.Error,
                               Message => "ESA acquire failed");
                     end if;

                  exception
                     when E : others =>
                        L.Log (Level   => L.Error,
                               Message => Ada.Exceptions.Exception_Information
                                 (E));
                  end;
               when others => null;
            end case;

            Xfrm.Thin.Rta_Next (Rta     => Rta'Access,
                                Attrlen => Rta_Size,
                                Address => Addr);
         end;
      end loop;
   end Process_Acquire;

   -------------------------------------------------------------------------

   procedure Process_Expire (Message : access Xfrm.Thin.Nlmsghdr_Type)
   is
      use type asm_generic_int_ll64_h.uu_u8;
      use type Tkmrpc.Results.Result_Type;

      Status : Tkmrpc.Results.Result_Type;
      Addr   : constant System.Address
        := Xfrm.Thin.Nlmsg_Data (Msg => Message);
      Expire : xfrm_h.xfrm_user_expire;
      for Expire'Address use Addr;
      pragma Import (Ada, Expire);
   begin
      L.Log (Message => "Initiating ESA expire (reqid" & Expire.state.reqid'Img
             & ", proto" & Expire.state.id.proto'Img
             & ", SPI" & Expire.state.id.spi'Img
             & ", hard " & Boolean'Image (Expire.hard /= 0) & ")");

      Tkmrpc.Clients.Ees.Esa_Expire
        (Result   => Status,
         Sp_Id    => Tkmrpc.Types.Sp_Id_Type (Expire.state.reqid),
         Spi_Rem  => Tkmrpc.Types.Esp_Spi_Type (Expire.state.id.spi),
         Protocol => Tkmrpc.Types.Protocol_Type (Expire.state.id.proto),
         Hard     => Tkmrpc.Types.Expiry_Flag_Type (Expire.hard));
      if Status /= Tkmrpc.Results.Ok then
         L.Log (Level   => L.Error,
                Message => "ESA expire failed");
      end if;

   exception
      when E : others =>
         L.Log (Level   => L.Error,
                Message => Ada.Exceptions.Exception_Information (E));
   end Process_Expire;

   -------------------------------------------------------------------------

   procedure Receiver_Error
     (E         :        Ada.Exceptions.Exception_Occurrence;
      Stop_Flag : in out Boolean)
   is
   begin
      L.Log (Level   => L.Error,
             Message => "Exception in Netlink receiver");
      L.Log (Level   => L.Error,
             Message => Ada.Exceptions.Exception_Information (X => E));
      Stop_Flag := False;
   end Receiver_Error;

end Callbacks;
