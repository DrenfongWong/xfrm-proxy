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

pragma License (GPL);

with Ada.Exceptions;

with Interfaces.C.Strings;

with Anet.Sockets.Netlink;
with Anet.Receivers.Datagram;

with Tkmrpc.Results;
with Tkmrpc.Clients.Ees;

with Callbacks;
with Version;
with Logger;

procedure Xfrm_Proxy
is

   use type Tkmrpc.Results.Result_Type;

   package L renames Logger;

   package Netlink_Receiver is new Anet.Receivers.Datagram
     (Socket_Type  => Anet.Sockets.Netlink.Raw_Socket_Type,
      Address_Type => Anet.Sockets.Netlink.Netlink_Addr_Type,
      Receive      => Anet.Sockets.Netlink.Receive);

   function C_Getpid return Interfaces.C.int;
   pragma Import (C, C_Getpid, "getpid");

   EES_Socket : constant String := "/tmp/tkm.rpc.ees";
   EES_Status : Tkmrpc.Results.Result_Type;

   Sock : aliased Anet.Sockets.Netlink.Raw_Socket_Type;
   Rcvr : Netlink_Receiver.Receiver_Type (S => Sock'Access);

   RPC_Error : exception;
begin
   L.Use_File;
   L.Log (Message => "XFRM Proxy starting (" & Version.Version_String & ")");

   Sock.Init (Protocol => Anet.Sockets.Netlink.Proto_Netlink_Xfrm);
   Sock.Bind (Address => Integer (C_Getpid),
              Groups  => (1 => Anet.Sockets.Netlink.Group_Xfrm_Acquire,
                          2 => Anet.Sockets.Netlink.Group_Xfrm_Expire));

   Rcvr.Register_Error_Handler (Callback => Callbacks.Receiver_Error'Access);
   Rcvr.Listen (Callback => Callbacks.Handle_Message'Access);

   declare
      Path : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (Str => EES_Socket);
   begin
      Tkmrpc.Clients.Ees.Init (Result  => EES_Status,
                               Address => Path);
      Interfaces.C.Strings.Free (Item => Path);
   end;

   if EES_Status /= Tkmrpc.Results.Ok then
      raise RPC_Error with "Could not bind to UNIX path " & EES_Socket;
   end if;
   L.Log (Message => "Initialization complete, waiting for kernel events");

exception
   when E : others =>
      L.Log (Level   => L.Error,
             Message => "Terminating due to error");
      L.Log (Level   => L.Error,
             Message => Ada.Exceptions.Exception_Information (X => E));
      Rcvr.Stop;
      L.Stop;
end Xfrm_Proxy;
