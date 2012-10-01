with Ada.Streams;
with Ada.Exceptions;

with Anet.Sockets.Netlink;

package Callbacks
is

   procedure Handle_Message
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Netlink.Netlink_Addr_Type);
   --  Handle Netlink message.

   procedure Receiver_Error
     (E         :        Ada.Exceptions.Exception_Occurrence;
      Stop_Flag : in out Boolean);
   --  Handle error in socket receiver. This handler just logs the exception
   --  occurrence and instructs the receiver to continue.

end Callbacks;
