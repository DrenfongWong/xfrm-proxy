with Ada.Streams;

with Anet.Sockets.Netlink;

package Callbacks
is

   procedure Handle_Message
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Netlink.Netlink_Addr_Type);
   --  Handle Netlink message.

end Callbacks;
