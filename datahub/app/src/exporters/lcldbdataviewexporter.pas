unit LCLDBDataViewExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataViewExporter, DataView;

type

  { TLCLDBDataViewExporter }

  TLCLDBDataViewExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

  { TLCLDataViewExporter }

  TLCLDataViewExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

implementation

uses
  LCLViewBuilder, Forms, LResources, Dialogs;

type
  TMyForm = class(TForm)

  end;

{ TLCLDataViewExporter }

class function TLCLDataViewExporter.Description: String;
begin
  Result := 'LCL View'
end;

class procedure TLCLDataViewExporter.Execute(DataView: TDataView);
var
  Form: TMyForm;
  Builder: TLCLViewBuilder;
  Stream: TMemoryStream;
  SaveDialog: TSaveDialog;
  FileName: String;
begin
  FileName := '';
  SaveDialog := TSaveDialog.Create(nil);
  try
    if SaveDialog.Execute then
      FileName := SaveDialog.FileName;
  finally
    SaveDialog.Destroy;
  end;
  if FileName = '' then
    Exit;
  Form := TMyForm.CreateNew(nil);
  Stream  := TMemoryStream.Create;
  Builder := TLCLViewBuilder.Create;
  try
    Form.Name := 'MyForm';
    Builder.Control := Form;
    Builder.Fields := DataView.Fields;

    Builder.Execute(Form);

    //Form.ShowModal;

    WriteComponentAsTextToStream(Stream, Form);
    Stream.SaveToFile('myform.lfm');

  finally
    Builder.Destroy;
    Stream.Destroy;
    Form.Destroy;
  end;
end;
{ TLCLDBDataViewExporter }

class function TLCLDBDataViewExporter.Description: String;
begin
  Result := 'LCL DB View'
end;

class procedure TLCLDBDataViewExporter.Execute(DataView: TDataView);
var
  Form: TMyForm;
  Builder: TLCLDBViewBuilder;
  Stream: TMemoryStream;
begin
  Form := TMyForm.CreateNew(nil);
  Stream  := TMemoryStream.Create;
  Builder := TLCLDBViewBuilder.Create;
  try
    Form.Name := 'MyForm';
    Builder.Control := Form;
    Builder.Fields := DataView.Fields;

    Builder.Execute(Form);

    //Form.ShowModal;

    WriteComponentAsTextToStream(Stream, Form);
    Stream.SaveToFile('myform.lfm');

  finally
    Builder.Destroy;
    Stream.Destroy;
    Form.Destroy;
  end;
end;

end.

