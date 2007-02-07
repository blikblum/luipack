unit dbutils;

{ Auxiliary classes or functions for database handling

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$Mode ObjFpc}
{$H+}
{ $Define DEBUG}

interface
uses 
  sysutils,classes,sax,sax_html,db, customsqliteds;

type
  TFieldArray = array of string;
  
  //Todo: 
// add StrUtils
// -add commandline
// -see redirection
// -add depth handling
//Handle RowState
//todo: close dataset in Destroy

    
  TDatapacketDumper = class (THtmlReader)
  private
    //FClearDataSet: boolean;
    FSourceFile:String;
    FDumpFieldDefs:Boolean;
    FDataset:TDataset;
    FAfterDumpFieldDefs:TProcedure;
    FOnApendRecord: TProcedure;
    FDestFields: TFieldArray;
    FSrcFields: TFieldArray;
    procedure EndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
    procedure StartElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure StartElementEx(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);	
    function String2FieldType(const AFieldName: String): TFieldType;
  public
    constructor Create;
    function DumpData:boolean;
    //property ClearDataSet: boolean read FCleardataset write FClearDataSet;
    property SrcFields: TFieldArray read FSrcFields write FSrcFields; 
    property DestFields: TFieldArray read FDestFields write FDestFields;
    property SourceFile:String read FSourceFile write FSourceFile;
    property Dataset:TDataset read FDataset write FDataset;
    property DumpFieldDefs: Boolean read FDumpFieldDefs write FDumpFieldDefs;
    property OnAppendRecord: TProcedure read FOnApendRecord write FOnApendRecord;   
    property AfterDumpFieldDefs: TProcedure read FAfterDumpFieldDefs write FAfterDumpFieldDefs;
  end;


  { TSqliteTree }
  TSqliteTree = class;

  TRecordNotify = procedure (Sender: TSqliteTree; HasChild: Boolean) of Object;
  TParentChangeNotify = procedure (Sender: TSqliteTree; Parent: Integer) of Object;

  TSqliteTree = class (TComponent)
  private
    FBreakRecursion: Boolean;
    FDataset: TCustomSqliteDataset;
    FFilter: String;
    FIndexFieldName: String;
    FInitialParent: Integer;
    FLevel: Integer;
    FOnParentChange: TParentChangeNotify;
    FOnRecord: TRecordNotify;
    FParentFieldName: String;
    FParentField: TField;
    FIndexField: TField;
    FTableName: String;
    FSqlTemplate: String;
    FHasChildSql: String;
    FData: Pointer;
    procedure GetChild(Parent: Integer);
  public
    //constructor Create (AOwner: TComponent); override;
    procedure Iterate(Recurse: Boolean = True);
    property Dataset: TCustomSqliteDataset read FDataset write FDataset;
    property TableName: String read FTableName write FTableName;
    property ParentFieldName: String read FParentFieldName write FParentFieldName;
    property IndexFieldName: String read FIndexFieldName write FIndexFieldName;
    property Filter: String read FFilter write FFilter;
    property Level: Integer read FLevel;
    property InitialParent: Integer read FInitialParent write FInitialParent;
    property Data: Pointer read FData write FData;
    property BreakRecursion: Boolean read FBreakRecursion write FBreakRecursion;
    property OnRecord: TRecordNotify read FOnRecord write FOnRecord;
    property OnParentChange: TParentChangeNotify read FOnParentChange write FOnParentChange;
  end;

implementation

//StrUtils.AnsiIndexStr complains with TFieldArray. This not.
function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;

var i : longint;

begin
  result:=-1;
  if high(AValues)=-1 Then
    Exit;
  for i:=low(AValues) to High(Avalues) do
     if (avalues[i]=AText) Then
       exit(i);					// make sure it is the first val.
end;

  { TDatapacketDumper }

constructor TDatapacketDumper.Create;
begin 
  inherited Create;
  OnStartElement:= @StartElement;
  OnEndElement:= @EndElement;
end;	  

procedure TDatapacketDumper.StartElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);	
var
  Counter:Integer;
begin
  //todo: use hardcoded positions ??
  if LocalName = 'row' then
  begin
    {$ifdef DEBUG}
    writeln('ARow');
    //if Atts.Localnames[0] <> 'rowstate' then
    //  writeln('ERROR - Rowstate is not the first att');
    {$endif}  
    Dataset.Append;  
    for counter:= 0 to Atts.Length - 1 do
    begin
      //Todo: substitute FieldByName
      {$ifdef DEBUG}
      writeln('FieldName: ',Atts.LocalNames[Counter]);
      writeln('FieldValue: ',Atts.Values[Counter]);
      {$endif}
      Dataset.FieldByName(Atts.LocalNames[Counter]).AsString:=Atts.Values[Counter];  
    end;
    Dataset.Post;  
  end else 
    if FDumpFieldDefs and (localname = 'field') then
    begin
      {$ifdef DEBUG}
      writeln('AField');
      writeln('FieldName: ',Atts.GetValue(NamespaceURI,'attrname'));
      writeln('FieldType: ',Atts.GetValue(NamespaceURI,'fieldtype'));
      {$endif}
      DataSet.FieldDefs.Add(Atts.GetValue(NamespaceURI,'attrname'),String2FieldType(Atts.GetValue(NamespaceURI,'fieldtype')));
    end; 
end;  	

procedure TDatapacketDumper.StartElementEx(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);	
var
  Counter:Integer;
  FieldNameIndex:Integer;
begin
  //todo: use hardcoded positions ??
  if LocalName = 'row' then
  begin
    writeln('ARow');
    //if Atts.Localnames[0] <> 'rowstate' then
    //  writeln('ERROR - Rowstate is not the first att');
    Dataset.Append;  
    for counter:= 0 to Atts.Length - 1 do //skip rowstate
    begin
      //Todo: substitute FieldByName
      writeln('FieldName: ',Atts.LocalNames[Counter]);
      writeln('FieldValue: ',Atts.Values[Counter]);
     
      FieldNameIndex:=AnsiIndexStr(Atts.LocalNames[Counter],FSrcFields);
      if FieldNameIndex <> -1 then
        Dataset.FieldByName(FDestFields[FieldNameIndex]).AsString:=Atts.Values[Counter];  
    end;
    Dataset.Post;  
  end else 
    if FDumpFieldDefs and (localname = 'field') then
    begin
      {$ifdef DEBUG}
      writeln('begin of field');
      writeln('FieldName: ',Atts.GetValue(NamespaceURI,'attrname'));
      writeln('FieldType: ',Atts.GetValue(NamespaceURI,'fieldtype'));
      {$endif}     
      FieldNameIndex:=AnsiIndexStr(Atts.GetValue(NamespaceURI,'attrname'),FSrcFields);
      if FieldNameIndex <> -1 then
        DataSet.FieldDefs.Add(FDestFields[FieldNameIndex],String2FieldType(Atts.GetValue(NamespaceURI,'fieldtype')));
    end; 
end;  	


procedure TDatapacketDumper.EndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  //Todo: set element handler to nil if end fields
  if LocalName = 'fields' then
  begin
    {$ifdef DEBUG}
    writeln('Entering end of fields');
    {$endif}
    if FDumpFieldDefs then
      FAfterDumpFieldDefs;  
    Dataset.Open;
    {$ifdef DEBUG}
    writeln('Exiting end of fields');
    {$endif}
  end;
end;

function TDatapacketDumper.DumpData:boolean;
begin
  Result:=False;
  if not FileExists(SourceFile) then
    raise Exception.Create('File '+SourceFile+' not found');
  if not Assigned(Dataset) then
    raise Exception.Create('Dataset property not set');
  if DumpFieldDefs and not Assigned(FAfterDumpFieldDefs) then
    raise Exception.Create('AfterDumpFieldDefs not set');
  Parse(SourceFile);
  Dataset.Close;
  Result:=True;   
end;  

function TDatapacketDumper.String2FieldType(const AFieldName: String): TFieldType;
begin
  case AnsiIndexStr(AFieldName,['i4','string','i2','boolean']) of
   0,2:  Result:=ftInteger;
   1:  Result:=ftString;
   //2:  Result:=ftSmallInt;
   3:  Result:=ftBoolean;
  else
    Result:=ftString; 
  end;  
end;  

{
  function String2FieldType(const AFieldName: String): TFieldType;
begin
  case AnsiIndexText(AFieldName,['integer','string','word',
  'autoinc','float','boolean',
  'date','datetime','time','memo']) of
   0:  Result:=ftInteger;
   1:  Result:=ftString;
   2:  Result:=ftWord;
   3:  Result:=ftAutoInc;
   4:  Result:=ftFloat;
   5:  Result:=ftBoolean;
   6:  Result:=ftDate;
   7:  Result:=ftDateTime;
   8:  Result:=ftTime;
   9:  Result:=ftMemo;
  else
    ShowMessage('Error - FieldType not Fount');
  end;
end;
}
 
{ TSqliteTree }

procedure TSqliteTree.GetChild(Parent: Integer);
var
  i,CurrentLinkValue: Integer;
  HasChildList: TFpList;
  HasChild: Boolean;
begin
  if Assigned(FOnParentChange) then
    FOnParentChange(Self,Parent);
  HasChildList:=TFPList.Create;
  with FDataset do
  begin
    Sql:=FSqlTemplate + IntToStr(Parent) + FFilter;
    RefetchData;
    while not Eof do
    begin
      CurrentLinkValue:=FIndexField.AsInteger;
      ExecuteDirect(FHasChildSql + IntToStr(CurrentLinkValue) + ' Limit 1');
      HasChild:=ReturnCode = 100 {SQLITE_ROW};
      FOnRecord(Self,HasChild);
      if HasChild then
        HasChildList.Add(Pointer(PtrInt(CurrentLinkValue)));
      Next;
    end;
  end;
  inc(FLevel);
  for i:= 0 to HasChildList.Count - 1 do
    if not FBreakRecursion then
      GetChild(PtrInt(HasChildList[i]));
  dec(FLevel);
  HasChildList.Destroy;
end;

{
constructor TSqliteTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
}

procedure TSqliteTree.Iterate(Recurse: Boolean = True);
begin
  if not Assigned(FOnRecord) then
    Raise Exception.Create('OnRecord notify function not set');
  with Dataset do
  begin
    FSqlTemplate:='Select * from '+FTableName+ ' Where '+ FParentFieldName + ' = ';
    FHasChildSql:='Select _ROWID_ from '+ FTableName+ ' Where '+FParentFieldName+ ' = ';
    //dummy open to allow refetchdata
    Sql:= 'Select * from '+FTableName+ ' Where 1 = 0';
    Close;
    Open;
    FParentField:=FieldByName(FParentFieldName);
    FIndexField:=FieldByName(FIndexFieldName);
    FLevel:=0;
    FBreakRecursion:=not Recurse;
    GetChild(FInitialParent);
  end;
end;

end.
