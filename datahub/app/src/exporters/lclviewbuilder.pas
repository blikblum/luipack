unit LCLViewBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Db, DbCtrls, StdCtrls, DataView, DataModel;

type

  { TCustomDirectLCLViewBuilder }

  TCustomDirectLCLViewBuilder = class
  private
    FPascalFile: TStringList;
    FLFMFile: TStringList;
    FView: TDataView;
  protected
    function GetUsedUnits: String; virtual;
    procedure WriteLFM(DataView: TDataView; Indentation: Integer); virtual;
    procedure WritePascal(DataView: TDataView; Indentation: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save(const BaseName: String);
    property View: TDataView read FView write FView;
  end;

  { TCustomLCLViewBuilder }

  TCustomLCLViewBuilder = class
  private
    FControl: TWinControl;
    FFields: TDataViewFields;
    FUnitFile: TStringList;
  protected
    procedure AddFieldEditorControl(AOwner: TComponent; Field: TDataViewField; var AnchorControl: TControl);
    procedure FieldEditorCreated(Field: TDataViewField; EditorControl: TWinControl); virtual;
    function GetFieldEditorClass(Field: TDataViewField; out HasCaption: Boolean; out Sufix: String): TWinControlClass; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(AOwner: TComponent);
    property Control: TWinControl read FControl write FControl;
    property Fields: TDataViewFields read FFields write FFields;
    property UnitFile: TStringList read FUnitFile;
  end;

  { TLCLDBViewBuilder }

  TLCLDBViewBuilder = class(TCustomLCLViewBuilder)
  private
    FDataSource: TDataSource;
  protected
    procedure FieldEditorCreated(Field: TDataViewField; EditorControl: TWinControl); override;
    function GetFieldEditorClass(Field: TDataViewField; out HasCaption: Boolean; out Sufix: String): TWinControlClass; override;
  public
    property DataSource: TDataSource read FDataSource write FDataSource;
  end;

  { TLCLViewBuilder }

  TLCLViewBuilder = class(TCustomLCLViewBuilder)
  private
  protected
    function GetFieldEditorClass(Field: TDataViewField; out HasCaption: Boolean; out Sufix: String): TWinControlClass; override;
  public
  end;

  { TLCLJSONBooleanGroupView }

  TLCLJSONBooleanGroupView = class(TCustomDirectLCLViewBuilder)
  protected
    procedure WriteLFM(DataView: TDataView; Indentation: Integer); override;
    procedure WritePascal(DataView: TDataView; Indentation: Integer); override;
    function GetUsedUnits: String; override;
  end;

implementation

uses
  LuiRTTIUtils, strutils;

{ TLCLJSONBooleanGroupView }

procedure TLCLJSONBooleanGroupView.WriteLFM(DataView: TDataView; Indentation: Integer);
var
  i: Integer;
  Field: TDataViewField;
  ScapedStr: String;
begin
  FLFMFile.Add(Space(Indentation) + 'object BooleanGroupMediator: TJSONBooleanGroupMediator');
  FLFMFile.Add(Space(Indentation + 2) + 'Properties = <');
  for i := 0 to DataView.Fields.Count - 1 do
  begin
    Field := DataView.Fields[i];
    FLFMFile.Add(Space(Indentation + 4) + 'item');
    ScapedStr := StringReplace(Field.DisplayLabel, '''', '''''', [rfReplaceAll]);
    FLFMFile.Add(Space(Indentation + 6) + 'Caption = ''' + ScapedStr + '''');
    FLFMFile.Add(Space(Indentation + 6) + 'Name = ''' + Field.FieldName + '''');
    FLFMFile.Add(Space(Indentation + 4) + 'end' + IfThen(i = DataView.Fields.Count -1, '>'));
  end;
  FLFMFile.Add(Space(Indentation) + 'end');
end;

procedure TLCLJSONBooleanGroupView.WritePascal(DataView: TDataView; Indentation: Integer);
begin
  FPascalFile.Add(Space(Indentation) + 'BooleanGroupMediator: TJSONBooleanGroupMediator;');
end;

function TLCLJSONBooleanGroupView.GetUsedUnits: String;
begin
  Result := inherited GetUsedUnits + ', fpjson, JSONMediators';
end;

{ TCustomDirectLCLViewBuilder }

function TCustomDirectLCLViewBuilder.GetUsedUnits: String;
begin
  Result := 'Classes, SysUtils, FileUtil, Forms, Controls, Graphics';
end;

procedure TCustomDirectLCLViewBuilder.WriteLFM(DataView: TDataView; Indentation: Integer);
begin
  //
end;

procedure TCustomDirectLCLViewBuilder.WritePascal(DataView: TDataView; Indentation: Integer);
begin
  //
end;

constructor TCustomDirectLCLViewBuilder.Create;
begin
  FLFMFile := TStringList.Create;
  FPascalFile := TStringList.Create;
end;

destructor TCustomDirectLCLViewBuilder.Destroy;
begin
  FLFMFile.Destroy;
  FPascalFile.Destroy;
  inherited Destroy;
end;

procedure TCustomDirectLCLViewBuilder.Save(const BaseName: String);
begin
  FPascalFile.Clear;
  FPascalFile.Add('unit MyForm;' + LineEnding);
  FPascalFile.Add('interface' + LineEnding);
  FPascalFile.Add('uses');
  FPascalFile.Add('  ' + GetUsedUnits + ';');
  FPascalFile.Add('');
  FPascalFile.Add('type');
  FPascalFile.Add('');
  FPascalFile.Add('  TMyForm = class(TForm)');

  WritePascal(View, 4);

  FPascalFile.Add('  private');
  FPascalFile.Add('  public');
  FPascalFile.Add('  end;');
  FPascalFile.Add('');
  FPascalFile.Add('var');
  FPascalFile.Add('  MyForm: TMyForm;');
  FPascalFile.Add('');
  FPascalFile.Add('implementation');
  FPascalFile.Add('');
  FPascalFile.Add('{$R *.lfm}');
  FPascalFile.Add('');
  FPascalFile.Add('end.');
  FPascalFile.SaveToFile(BaseName + '.pas');

  FLFMFile.Clear;
  FLFMFile.Add('object MyForm: TMyForm');
  FLFMFile.Add('  Left = 300');
  FLFMFile.Add('  Height = 400');
  FLFMFile.Add('  Top = 200');
  FLFMFile.Add('  Width = 500');
  FLFMFile.Add('  LCLVersion = ''1.1''');
  WriteLFM(View, 2);
  FLFMFile.Add('end');
  FLFMFile.SaveToFile(BaseName + '.lfm');
end;

{ TLCLViewBuilder }

function TLCLViewBuilder.GetFieldEditorClass(Field: TDataViewField; out
  HasCaption: Boolean; out Sufix: String): TWinControlClass;
begin
  case Field.FieldType of
    dftDate, dftTime, dftDateTime, dftFloat, dftString, dftInteger:
    begin
      Result := TEdit;
      HasCaption := False;
      Sufix := 'Edit';
    end;
  end;
end;

{ TLCLDBViewBuilder }

procedure TLCLDBViewBuilder.FieldEditorCreated(Field: TDataViewField; EditorControl: TWinControl);
begin
  SetObjectProperties(EditorControl, [
    'DataField', Field.FieldName,
    'DataSource', FDataSource
    ]);
end;

function TLCLDBViewBuilder.GetFieldEditorClass(Field: TDataViewField; out
  HasCaption: Boolean; out Sufix: String): TWinControlClass;
begin
  case Field.FieldType of
    dftDate, dftTime, dftDateTime, dftFloat, dftString, dftInteger:
    begin
      Result := TDBEdit;
      HasCaption := False;
      Sufix := 'Edit';
    end;
  end;
end;

{ TCustomLCLViewBuilder }

procedure TCustomLCLViewBuilder.AddFieldEditorControl(AOwner: TComponent; Field: TDataViewField; var AnchorControl: TControl);
var
  EditorControl: TWinControl;
  DisplayLabel: TLabel;
  HasCaption: Boolean;
  NewTop: Integer;
  ControlClass: TWinControlClass;
  ControlNameSufix: String;
begin
  DisplayLabel := nil;
  ControlClass := GetFieldEditorClass(Field, HasCaption, ControlNameSufix);
  EditorControl := ControlClass.Create(AOwner);
  EditorControl.Name := Field.FieldName + ControlNameSufix;
  if HasCaption then
    EditorControl.Caption := Field.DisplayLabel;

  FUnitFile.Add('    ' + EditorControl.Name + ': ' + EditorControl.ClassName + ';');

  //FControl.DisableAutoSizing;
  if not HasCaption then
  begin
    DisplayLabel := TLabel.Create(AOwner);
    DisplayLabel.Parent := FControl;
    DisplayLabel.Caption := Field.DisplayLabel;
    DisplayLabel.Visible := True;
    DisplayLabel.Name := Field.FieldName + 'Label';
    DisplayLabel.Anchors := [akBottom, akLeft];

    FUnitFile.Add('    ' + DisplayLabel.Name + ': TLabel;');


    if AnchorControl <> nil then
    begin
      //DisplayLabel.AnchorParallel(akLeft, 0, FAnchorControl);
      //DisplayLabel.AnchorToNeighbour(akTop, 2, FAnchorControl);
      //remove anchors. We just need the initial position.
      //DisplayLabel.AnchorParallel(akLeft, 0, nil);
      //DisplayLabel.AnchorToNeighbour(akTop, 2, nil);
      DisplayLabel.Top := AnchorControl.Top + AnchorControl.Height + 2;
    end;
    //DisplayLabel.Left := 0;
    AnchorControl := DisplayLabel;
  end;

  EditorControl.Parent := FControl;
  if (AnchorControl <> nil) then
  begin
    NewTop := AnchorControl.Top + AnchorControl.Height;
    if AnchorControl <> DisplayLabel then
      Inc(NewTop, 2);
    EditorControl.Top := NewTop;
    //EditorControl.AnchorToNeighbour(akTop, 2, FAnchorControl);
    //EditorControl.AnchorParallel(akLeft, 0, FAnchorControl);
    //remove anchors. We just need the initial position.
    //EditorControl.AnchorToNeighbour(akTop, 0, nil);
    //EditorControl.AnchorParallel(akLeft, 0, nil);
  end;

  if not HasCaption then
  begin
    DisplayLabel.AnchorParallel(akLeft, 0, EditorControl);
    DisplayLabel.AnchorToNeighbour(akBottom, 0, EditorControl);
  end;
  AnchorControl := EditorControl;
  EditorControl.Visible := True;
  FieldEditorCreated(Field, EditorControl);
end;

procedure TCustomLCLViewBuilder.FieldEditorCreated(Field: TDataViewField; EditorControl: TWinControl);
begin
  //
end;

constructor TCustomLCLViewBuilder.Create;
begin
  inherited;
  FUnitFile := TStringList.Create;
end;

destructor TCustomLCLViewBuilder.Destroy;
begin
  FUnitFile.Destroy;
  inherited Destroy;
end;

procedure TCustomLCLViewBuilder.Execute(AOwner: TComponent);
var
  i: Integer;
  AnchorControl: TControl;
begin
  if FControl = nil then
    raise Exception.Create('TDBControlBuilder - FControl = nil');
  if FFields = nil then
    raise Exception.Create('TDBControlBuilder - FFields = nil');

  FUnitFile.Clear;
  FUnitFile.Add('unit MyForm;' + LineEnding);
  FUnitFile.Add('interface' + LineEnding);
  FUnitFile.Add('uses');
  FUnitFile.Add('  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Db;');
  FUnitFile.Add('');
  FUnitFile.Add('type');
  FUnitFile.Add('');
  FUnitFile.Add('  TMyForm = class(TForm)');

  AnchorControl := nil;
  for i := 0 to FFields.Count - 1 do
  begin
    AddFieldEditorControl(AOwner, FFields[i], AnchorControl);
  end;

  FUnitFile.Add('  private');
  FUnitFile.Add('    { private declarations }');
  FUnitFile.Add('  public');
  FUnitFile.Add('    { public declarations }');
  FUnitFile.Add('  end;');
  FUnitFile.Add('');
  FUnitFile.Add('var');
  FUnitFile.Add('  MyForm: TMyForm;');
  FUnitFile.Add('');
  FUnitFile.Add('implementation');
  FUnitFile.Add('');
  FUnitFile.Add('{$R *.lfm}');
  FUnitFile.Add('');
  FUnitFile.Add('end.');
end;

end.

