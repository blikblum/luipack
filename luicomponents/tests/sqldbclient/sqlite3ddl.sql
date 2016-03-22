CREATE TABLE [Categories] (
  [Id] INTEGER PRIMARY KEY, 
  [Name] VARCHAR);


CREATE TABLE [Contacts] (
  [Id] INTEGER PRIMARY KEY, 
  [CategoryId] INTEGER REFERENCES [Categories]([Id]) ON DELETE SET NULL, 
  [Name] VARCHAR);


CREATE TABLE [ContactDetails] (
  [ContactId] INTEGER PRIMARY KEY REFERENCES [Contacts]([Id]) ON DELETE CASCADE,
  [ObjectData] TEXT,
  [ArrayData] TEXT);


CREATE TABLE [Phones] (
  [Id] INTEGER PRIMARY KEY, 
  [ContactId] INTEGER REFERENCES [Contacts]([Id]) ON DELETE CASCADE, 
  [Number] VARCHAR);


CREATE TABLE SystemData (
  [Key] VARCHAR,
  [Value] TEXT,
  CONSTRAINT [] PRIMARY KEY ( [Key] ));


