package Logger
is
   type Log_Level is
     (Debug,
      Info,
      Notice,
      Warning,
      Error,
      Critical,
      Alert,
      Emergency);

   procedure Log
     (Level   : Log_Level := Info;
      Message : String);
   --  Log the specified message with given loglevel.

   procedure Use_File (Path : String := "");
   --  Switch to file based logging. If no file is given, stdout is used.

   procedure Stop;
   --  Stop logger.

end Logger;
