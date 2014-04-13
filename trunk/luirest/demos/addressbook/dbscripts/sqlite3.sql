CREATE TABLE [Categories] (
  [Id] INTEGER PRIMARY KEY, 
  [Name] VARCHAR);


CREATE TABLE [Contacts] (
  [Id] INTEGER PRIMARY KEY, 
  [CategoryId] INTEGER REFERENCES [Categories]([Id]) ON DELETE SET NULL, 
  [Name] VARCHAR);


CREATE TABLE [Phones] (
  [Id] INTEGER PRIMARY KEY, 
  [ContactId] INTEGER REFERENCES [Contacts]([Id]) ON DELETE CASCADE, 
  [Number] VARCHAR);


