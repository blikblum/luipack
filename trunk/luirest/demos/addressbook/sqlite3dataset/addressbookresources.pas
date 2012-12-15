unit AddressBookResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, Sqlite3DS, HTTPDefs, fphttp, sqlite3, db;

type

  { TXMLRESTResponseFormatter }

  TXMLRESTResponseFormatter = class(TRESTResponseFormatter)
  public
    class procedure SetStatus(AResponse: TResponse; StatusCode: Integer; const Message: String;
      const Args: array of const); override;
  end;

  { TDatasetResourceFormatter }

  TDatasetResourceFormatter = class
  public
    class procedure SetContent(AResponse: TResponse; Dataset: TDataset; IsCollection: Boolean); virtual;
  end;

  TDatasetResourceFormatterClass = class of TDatasetResourceFormatter;

  { TJSONDatasetResourceFormatter }

  TJSONDatasetResourceFormatter = class(TDatasetResourceFormatter)
  public
    class procedure SetContent(AResponse: TResponse; Dataset: TDataset; IsCollection: Boolean); override;
  end;

  { TXMLDatasetResourceFormatter }

  TXMLDatasetResourceFormatter = class(TDatasetResourceFormatter)
  public
    class procedure SetContent(AResponse: TResponse; Dataset: TDataset; IsCollection: Boolean); override;
  end;

  { TSqlite3DatasetResource }

  TSqlite3DatasetResource = class(TRESTResource)
  private
    FDataset: TSqlite3Dataset;
  public
    class var DatasetFormatter: TDatasetResourceFormatterClass;
  published
    property Dataset: TSqlite3Dataset read FDataset write FDataset;
  end;

  { TContact }

  TContact = class(TSqlite3DatasetResource)
  public
    procedure AfterConstruction; override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TContacts }

  TContacts = class(TSqlite3DatasetResource)
  public
    procedure AfterConstruction; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); override;
  end;


  { TContactPhone }

  TContactPhone = class(TSqlite3DatasetResource)
  public
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TContactPhones }

  TContactPhones = class(TSqlite3DatasetResource)
  public
    procedure AfterConstruction; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

uses
  LuiJSONUtils, fpjson, XMLWrite, DOM;

{ TXMLDatasetResourceFormatter }

class procedure TXMLDatasetResourceFormatter.SetContent(AResponse: TResponse;
  Dataset: TDataset; IsCollection: Boolean);
var
  XMLDoc: TXMLDocument;
  RootNode, RecordNode, FieldNode: TDOMNode;
  i: Integer;
  Field: TField;
  Stream: TMemoryStream;
begin
  XMLDoc := TXMLDocument.Create;
  RootNode := XMLDoc.CreateElement('response');
  Dataset.First;
  while not Dataset.EOF do
  begin
    RecordNode := XMLDoc.CreateElement('record');
    for i := 0 to Dataset.Fields.Count -1 do
    begin
      Field := Dataset.Fields[i];
      FieldNode := XMLDoc.CreateElement(Field.FieldName);
      FieldNode.AppendChild(XMLDoc.CreateTextNode(Field.AsString));
      RecordNode.AppendChild(FieldNode);
    end;
    RootNode.AppendChild(RecordNode);
    Dataset.Next;
  end;
  XMLDoc.AppendChild(RootNode);
  Stream := TMemoryStream.Create;
  WriteXMLFile(XMLDoc, Stream);
  //todo: fix the leak of stream
  AResponse.ContentStream := Stream;
  XMLDoc.Destroy;
end;
{ TJSONDatasetResourceFormatter }

class procedure TJSONDatasetResourceFormatter.SetContent(AResponse: TResponse;
  Dataset: TDataset; IsCollection: Boolean);
var
  ResponseData: TJSONData;
  Options: TDatasetToJSONOptions;
begin
  if IsCollection then
    Options := [djoSetNull]
  else
    Options := [djoCurrentRecord, djoSetNull];
  ResponseData := DatasetToJSONData(Dataset, Options, '');
  try
    AResponse.Contents.Add(ResponseData.AsJSON);
  finally
    ResponseData.Free;
  end;
end;

{ TDatasetResourceFormatter }

class procedure TDatasetResourceFormatter.SetContent(AResponse: TResponse;
  Dataset: TDataset; IsCollection: Boolean);
begin
  //
end;

{ TXMLRESTResponseFormatter }

class procedure TXMLRESTResponseFormatter.SetStatus(AResponse: TResponse;
  StatusCode: Integer; const Message: String; const Args: array of const);
var
  XMLDoc: TXMLDocument;
  RootNode, MessageNode: TDOMNode;
  Stream: TMemoryStream;
begin
  AResponse.Code := StatusCode;
  XMLDoc := TXMLDocument.Create;
  RootNode := XMLDoc.CreateElement('response');
  MessageNode := XMLDoc.CreateElement('message');
  MessageNode.AppendChild(XMLDoc.CreateTextNode(Format(Message, Args)));
  RootNode.AppendChild(MessageNode);
  XMLDoc.AppendChild(RootNode);
  //todo fix leak of stream
  Stream := TMemoryStream.Create;
  WriteXMLFile(XMLDoc, Stream);
  AResponse.ContentStream := Stream;
  XMLDoc.Destroy;
end;

{ TContactPhone }

procedure TContactPhone.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.ExecSQL(Format('Delete from Phones where Id = %s', [URIParams.Strings['phoneid']]));
end;

procedure TContactPhone.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.SQL := Format('Select Id, Number From Phones Where Id = %s',
    [URIParams.Strings['phoneid']]);
  Dataset.Close;
  Dataset.Open;
  if Dataset.RecordCount > 0 then
  begin
    DatasetFormatter.SetContent(AResponse, Dataset, False);
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Contents.Add(Format('Phone "%s" not found', [URIParams.Strings['phoneid']]));
  end;
end;

procedure TContactPhone.HandlePut(ARequest: TRequest; AResponse: TResponse);
const
  UpdateSQL = 'Update Phones Set Number = ''%s'' where Id = %s';
var
  RequestData: TJSONObject;
begin
  RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
  try
    Dataset.ExecSQL(Format(UpdateSQL, [RequestData.Strings['number'],
      URIParams.Strings['phoneid']]));
    if Dataset.ReturnCode = SQLITE_DONE then
    begin
      if Dataset.RowsAffected > 0 then
      begin
        Dataset.Close;
        Dataset.SQL := Format('Select Id, Number From Phones where Id = %s',
          [URIParams.Strings['phoneid']]);
        Dataset.Open;
        DatasetFormatter.SetContent(AResponse, Dataset, False);
      end
      else
      begin
        SetResponseStatus(AResponse, 404, 'Phone "%s" not found', [URIParams.Strings['phoneid']]);
      end;
    end
    else
    begin
      SetResponseStatus(AResponse, 500, 'Error updating resource', []);
    end;
  finally
    RequestData.Destroy;
  end;
end;

{ TContactPhones }

procedure TContactPhones.AfterConstruction;
begin
  inherited AfterConstruction;
  SetDefaultSubPath('phoneid', TContactPhone, 0);
end;

procedure TContactPhones.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.SQL := Format('Select Id, Number From Phones where ContactId = %s', [URIParams.Strings['contactid']]);
  Dataset.Close;
  Dataset.Open;
  DatasetFormatter.SetContent(AResponse, Dataset, True);
end;

procedure TContactPhones.HandlePost(ARequest: TRequest; AResponse: TResponse);
const
  InsertSQL = 'Insert Into Phones (ContactId, Number) Values (%s, ''%s'')';
var
  RequestData: TJSONObject;
begin
  RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
  try
    Dataset.ExecSQL(Format(InsertSQL, [URIParams.Strings['contactid'],
      RequestData.Strings['number']]));
    if Dataset.ReturnCode = SQLITE_DONE then
    begin
      Dataset.Close;
      Dataset.SQL := Format('Select Id, Number From Phones where Id = %d',
        [Dataset.LastInsertRowId]);
      Dataset.Open;
      DatasetFormatter.SetContent(AResponse, Dataset, True);
    end
    else
    begin
      AResponse.Code := 400;
      AResponse.Contents.Add('Unable too add phone ' + Dataset.ReturnString);
    end;
  finally
    RequestData.Destroy;
  end;
end;

{ TContacts }

procedure TContacts.AfterConstruction;
begin
  inherited AfterConstruction;
  SetDefaultSubPath('contactid', TContact, 0);
end;

procedure TContacts.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.SQL := 'Select Id, Name From Contacts';
  Dataset.Close;
  Dataset.Open;
  DatasetFormatter.SetContent(AResponse, Dataset, True);
end;

procedure TContacts.HandlePost(ARequest: TRequest; AResponse: TResponse);
const
  InsertSQL = 'Insert Into Contacts (Name) Values (''%s'')';
var
  RequestData: TJSONObject;
begin
  RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
  try
    Dataset.ExecSQL(Format(InsertSQL, [RequestData.Strings['name']]));
    if Dataset.ReturnCode = SQLITE_DONE then
    begin
      Dataset.Close;
      Dataset.SQL := Format('Select Id, Name From Contacts where Id = %d',
        [Dataset.LastInsertRowId]);
      Dataset.Open;
      DatasetFormatter.SetContent(AResponse, Dataset, False);
    end
    else
    begin
      AResponse.Code := 400;
      AResponse.Contents.Add('Unable too add contact ' + Dataset.ReturnString);
    end;
  finally
    RequestData.Destroy;
  end;
end;

{ TContact }

procedure TContact.AfterConstruction;
begin
  inherited AfterConstruction;
  RegisterSubPath('phones', TContactPhones, 0);
end;

procedure TContact.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.ExecSQL(Format('Delete from Contacts where Id = %s', [URIParams.Strings['contactid']]));
end;

procedure TContact.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.SQL := Format('Select Id, Name From Contacts where Id = %s',
    [URIParams.Strings['contactid']]);
  Dataset.Close;
  Dataset.Open;
  if Dataset.RecordCount > 0 then
  begin
    DatasetFormatter.SetContent(AResponse, Dataset, False);
  end
  else
  begin
    SetResponseStatus(AResponse, 404, 'Contact "%s" not found', [URIParams.Strings['contactid']]);
  end;
end;

procedure TContact.HandlePut(ARequest: TRequest; AResponse: TResponse);
const
  UpdateSQL = 'Update Contacts Set Name = ''%s'' where Id = %s';
var
  RequestData: TJSONObject;
begin
  RequestData := StringToJSONData(ARequest.Content) as TJSONObject;
  try
    Dataset.ExecSQL(Format(UpdateSQL, [RequestData.Strings['name'],
      URIParams.Strings['contactid']]));
    if Dataset.ReturnCode = SQLITE_DONE then
    begin
      if Dataset.RowsAffected > 0 then
      begin
        Dataset.Close;
        Dataset.SQL := Format('Select Id, Name From Contacts where Id = %s',
          [URIParams.Strings['contactid']]);
        Dataset.Open;
        DatasetFormatter.SetContent(AResponse, Dataset, False);
      end
      else
      begin
        SetResponseStatus(AResponse, 404, 'Contact "%s" not found', [URIParams.Strings['contactid']]);
      end;
    end
    else
    begin
      SetResponseStatus(AResponse, 500, 'Error updating resource', []);
    end;
  finally
    RequestData.Destroy;
  end;
end;

end.

