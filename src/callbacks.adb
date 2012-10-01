with Ada.Exceptions;

with System;

with asm_generic_int_ll64_h;
with xfrm_h;

with Tkmrpc.Types;
with Tkmrpc.Results;
with Tkmrpc.Clients.Ees;

with Xfrm;

with Logger;

package body Callbacks
is

   package L renames Logger;

   procedure Process_Acquire (Message : access Xfrm.Nlmsghdr_Type);
   --  Process Kernel ACQUIRE message.

   procedure Process_Expire (Message : access Xfrm.Nlmsghdr_Type);
   --  Process Kernel EXPIRE message.

   -------------------------------------------------------------------------

   procedure Handle_Message
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Netlink.Netlink_Addr_Type)
   is
      Msg : aliased Xfrm.Nlmsghdr_Type;

      for Msg'Address use Item'Address;

      Action : Xfrm.Xfrm_Msg_Type;
   begin
      if not Xfrm.Nlmsg_Ok (Msg => Msg,
                            Len => Item'Length)
      then
         L.Log (Message => "Ignoring invalid Netlink message from pid"
                & Src'Img);
         return;
      end if;

      Action := Xfrm.Xfrm_Msg_Type'Enum_Val (Msg.Nlmsg_Type);
      L.Log (Message => "Netlink message received (" & Item'Length'Img
             & " bytes ) from pid" & Src'Img & ", type " & Action'Img);

      case Action is
         when Xfrm.XFRM_MSG_ACQUIRE => Process_Acquire (Message => Msg'Access);
         when Xfrm.XFRM_MSG_EXPIRE  => Process_Expire (Message => Msg'Access);
         when others                => null;
      end case;
   end Handle_Message;

   -------------------------------------------------------------------------

   procedure Process_Acquire (Message : access Xfrm.Nlmsghdr_Type)
   is
      Addr     : System.Address
        := Xfrm.Nlmsg_Data
          (Msg => Message,
           Len => xfrm_h.xfrm_user_acquire'Size / 8);
      Rta_Size : Natural
        := Xfrm.Nlmsg_Payload
          (Msg => Message.all,
           Len => xfrm_h.xfrm_user_acquire'Size / 8);
   begin
      loop
         declare
            Attr_Kind : xfrm_h.xfrm_attr_type_t;
            Rta       : aliased Xfrm.Rtattr_Type;
            for Rta'Address use Addr;
            pragma Import (Ada, Rta);
         begin
            exit when not Xfrm.Rta_Ok
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
                       := Xfrm.Rta_Data (Rta => Rta'Access);
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

            Xfrm.Rta_Next (Rta     => Rta'Access,
                           Attrlen => Rta_Size,
                           Address => Addr);
         end;
      end loop;
   end Process_Acquire;

   -------------------------------------------------------------------------

   procedure Process_Expire (Message : access Xfrm.Nlmsghdr_Type)
   is
      use type asm_generic_int_ll64_h.uu_u8;
      use type Tkmrpc.Results.Result_Type;

      Status : Tkmrpc.Results.Result_Type;
      Addr   : constant System.Address
        := Xfrm.Nlmsg_Data (Msg => Message);
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
