CREATE TABLE Categories (
  Id SERIAL PRIMARY KEY, 
  Name VARCHAR);

CREATE TABLE Contacts (
  Id SERIAL PRIMARY KEY, 
  CategoryId INTEGER  REFERENCES Categories(Id) ON DELETE SET NULL, 
  Name VARCHAR);

CREATE TABLE Phones (
  Id SERIAL PRIMARY KEY, 
  ContactId INTEGER REFERENCES Contacts(Id) ON DELETE CASCADE, 
  Number VARCHAR);
  
GRANT ALL ON TABLE categories TO public;
GRANT ALL ON TABLE contacts TO public;
GRANT ALL ON TABLE phones TO public;

GRANT ALL ON TABLE categories_id_seq TO public;
GRANT ALL ON TABLE contacts_id_seq TO public;
GRANT ALL ON TABLE phones_id_seq TO public;