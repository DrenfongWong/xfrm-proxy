with Ada.Text_IO;
with Ada.Exceptions;

with Interfaces.C.Strings;

with Anet.Sockets.Netlink;
with Anet.Receivers.Datagram;

with Tkmrpc.Results;
with Tkmrpc.Clients.Ees;

with Callbacks;

procedure Xfrm_Proxy
is

   use type Tkmrpc.Results.Result_Type;

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
   Sock.Init (Protocol => Anet.Sockets.Netlink.Proto_Netlink_Xfrm);
   Sock.Bind (Address => Integer (C_Getpid),
              Groups  => (1 => Anet.Sockets.Netlink.Group_Xfrm_Acquire,
                          2 => Anet.Sockets.Netlink.Group_Xfrm_Expire));

   Rcvr.Listen (Callback => Callbacks.Handle_Message'Access);

   Tkmrpc.Clients.Ees.Init (Result  => EES_Status,
                            Address => Interfaces.C.Strings.New_String
                              (EES_Socket));
   if EES_Status /= Tkmrpc.Results.Ok then
      raise RPC_Error with "Could not bind to UNIX path " & EES_Socket;
   end if;

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Exception caught:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (X => E));
      Rcvr.Stop;
end Xfrm_Proxy;
