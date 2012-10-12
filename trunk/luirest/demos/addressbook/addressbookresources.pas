unit AddressBookResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiREST, Sqlite3DS, HTTPDefs, fphttp, sqlite3;

type

  { TSqlite3DatasetResource }

  TSqlite3DatasetResource = class(TCustomRESTResource)
  private
    FDataset: TSqlite3Dataset;
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
  LuiJSONUtils, fpjson;

{ TContactPhone }

procedure TContactPhone.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.ExecSQL(Format('Delete from Phones where Id = %s', [URIParams.Strings['phoneid']]));
end;

procedure TContactPhone.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  ResponseData: TJSONData;
begin
  Dataset.SQL := Format('Select Id, Number From Phones Where Id = %s',
    [URIParams.Strings['phoneid']]);
  Dataset.Close;
  Dataset.Open;
  if Dataset.RecordCount > 0 then
  begin
    ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord, djoSetNull], '');
    try
      AResponse.Contents.Add(ResponseData.AsJSON);
    finally
      ResponseData.Free;
    end;
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
  ResponseData: TJSONData;
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
        ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord], '');
        try
          AResponse.Contents.Add(ResponseData.AsJSON);
        finally
          ResponseData.Destroy;
        end;
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
  SetDefaultSubResource('phoneid', nil, TContactPhone);
end;

procedure TContactPhones.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  ResponseData: TJSONData;
begin
  Dataset.SQL := Format('Select Id, Number From Phones where ContactId = %s', [URIParams.Strings['contactid']]);
  Dataset.Close;
  Dataset.Open;
  ResponseData := DatasetToJSONData(Dataset, [djoSetNull], '');
  try
    AResponse.Contents.Add(ResponseData.AsJSON);
  finally
    ResponseData.Free;
  end;
end;

procedure TContactPhones.HandlePost(ARequest: TRequest; AResponse: TResponse);
const
  InsertSQL = 'Insert Into Phones (ContactId, Number) Values (%s, ''%s'')';
var
  RequestData: TJSONObject;
  ResponseData: TJSONData;
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
      ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord], '');
      try
        AResponse.Contents.Add(ResponseData.AsJSON);
      finally
        ResponseData.Destroy;
      end;
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
  SetDefaultSubResource('contactid', nil, TContact);
end;

procedure TContacts.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  ResponseData: TJSONData;
begin
  Dataset.SQL := 'Select Id, Name From Contacts';
  Dataset.Close;
  Dataset.Open;
  ResponseData := DatasetToJSONData(Dataset, [djoSetNull], '');
  try
    AResponse.Contents.Add(ResponseData.AsJSON);
  finally
    ResponseData.Free;
  end;
end;

procedure TContacts.HandlePost(ARequest: TRequest; AResponse: TResponse);
const
  InsertSQL = 'Insert Into Contacts (Name) Values (''%s'')';
var
  RequestData: TJSONObject;
  ResponseData: TJSONData;
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
      ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord], '');
      try
        AResponse.Contents.Add(ResponseData.AsJSON);
      finally
        ResponseData.Destroy;
      end;
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
  RegisterSubPathResource('phones', nil, TContactPhones);
end;

procedure TContact.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  Dataset.ExecSQL(Format('Delete from Contacts where Id = %s', [URIParams.Strings['contactid']]));
end;

procedure TContact.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  ResponseData: TJSONData;
begin
  Dataset.SQL := Format('Select Id, Name From Contacts where Id = %s',
    [URIParams.Strings['contactid']]);
  Dataset.Close;
  Dataset.Open;
  if Dataset.RecordCount > 0 then
  begin
    ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord, djoSetNull], '');
    try
      AResponse.Contents.Add(ResponseData.AsJSON);
    finally
      ResponseData.Free;
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Contents.Add(Format('Contact "%s" not found', [URIParams.Strings['contactid']]));
  end;
end;

procedure TContact.HandlePut(ARequest: TRequest; AResponse: TResponse);
const
  UpdateSQL = 'Update Contacts Set Name = ''%s'' where Id = %s';
var
  RequestData: TJSONObject;
  ResponseData: TJSONData;
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
        ResponseData := DatasetToJSONData(Dataset, [djoCurrentRecord], '');
        try
          AResponse.Contents.Add(ResponseData.AsJSON);
        finally
          ResponseData.Destroy;
        end;
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

