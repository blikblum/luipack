CREATE TABLE [Event] (
  [Id] INTEGER PRIMARY KEY, 
  [Date] DATE, 
  [AppName] VARCHAR, 
  [AppVersion] VARCHAR, 
  [AppLog] TEXT, 
  [UserDescription] TEXT, 
  [Type] VARCHAR, 
  [Active] BOOLEAN DEFAULT 1);

