with Alog.Facilities.Syslog;
with Alog.Facilities.File_Descriptor;
with Alog.Tasked_Logger;
with Alog.Active_Logger;

pragma Elaborate_All (Alog.Active_Logger);
pragma Elaborate_All (Alog.Tasked_Logger);
pragma Elaborate_All (Alog.Facilities.Syslog);

pragma Unreferenced (Alog.Tasked_Logger);

package body Logger
is
   Instance : Alog.Active_Logger.Instance (Init => False);

   Level_Map : constant array (Log_Level) of Alog.Log_Level
     := (Debug     => Alog.Debug,
         Info      => Alog.Info,
         Notice    => Alog.Notice,
         Warning   => Alog.Warning,
         Error     => Alog.Error,
         Critical  => Alog.Critical,
         Alert     => Alog.Alert,
         Emergency => Alog.Emergency);

   -------------------------------------------------------------------------

   procedure Log
     (Level   : Log_Level := Info;
      Message : String)
   is
   begin
      Instance.Log_Message (Level => Level_Map (Level),
                            Msg   => Message);
   end Log;

   -------------------------------------------------------------------------

   procedure Stop
   is
   begin
      Instance.Shutdown;
   end Stop;

   -------------------------------------------------------------------------

   procedure Use_File (Path : String := "")
   is
      use Alog.Facilities;

      F : constant Handle := new File_Descriptor.Instance;
   begin
      if Path'Length > 0 then
         File_Descriptor.Handle (F).Set_Logfile
           (Path   => Path,
            Append => False);
      end if;

      Instance.Clear;
      Instance.Attach_Facility (Facility => F);
   end Use_File;

   -------------------------------------------------------------------------

begin
   declare
      use Alog.Facilities;

      S : constant Handle := new Syslog.Instance;
   begin
      S.Toggle_Write_Timestamp (State => False);
      Syslog.Handle (S).Set_Origin (Value => Syslog.LOG_DAEMON);
      Instance.Attach_Facility (Facility => S);
   end;
end Logger;
