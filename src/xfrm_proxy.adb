with Ada.Text_IO;
with Ada.Exceptions;

with Interfaces.C;

with Anet.Sockets.Netlink;
with Anet.Sockets.Dgram_Receiver;

with Callbacks;

procedure Xfrm_Proxy
is

   package Netlink_Receiver is new Anet.Sockets.Dgram_Receiver
     (Socket_Type  => Anet.Sockets.Netlink.Raw_Socket_Type,
      Address_Type => Anet.Sockets.Netlink.Netlink_Addr_Type,
      Receive      => Anet.Sockets.Netlink.Receive);

   function C_Getpid return Interfaces.C.int;
   pragma Import (C, C_Getpid, "getpid");

   Sock : aliased Anet.Sockets.Netlink.Raw_Socket_Type;
   Rcvr : Netlink_Receiver.Receiver_Type (S => Sock'Access);
begin
   Sock.Init (Protocol => Anet.Sockets.Netlink.Proto_Netlink_Xfrm);
   Sock.Bind (Address => Integer (C_Getpid),
              Groups  => (1 => Anet.Sockets.Netlink.Group_Xfrm_Acquire,
                          2 => Anet.Sockets.Netlink.Group_Xfrm_Expire));

   Rcvr.Listen (Callback => Callbacks.Handle_Message'Access);

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Exception caught:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (X => E));
      Rcvr.Stop;
end Xfrm_Proxy;
