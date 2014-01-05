unit HTMLFormExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataViewExporter, DataModel, DataView;

type

  { THTMLDataViewExporter }

  THTMLDataViewExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

implementation

{ THTMLDataViewExporter }

class function THTMLDataViewExporter.Description: String;
begin
  Result := 'HTML Form';
end;

class procedure THTMLDataViewExporter.Execute(DataView: TDataView);
const
  FieldHTMLTemplate = '  <div class="form-group">' + LineEnding +
    '    <label for="%s">%s</label>' + LineEnding +
    '    <input type="text" class="form-control" name="%s" id="%s" value="{{%s}}">'+ LineEnding+
    '  </div>';
var
  Output: TStringList;
  Field: TDataViewField;
  FieldHTML, FieldHTMLId: String;
  i: Integer;
begin
  //todo: use DOMTree
  //todo: implement options: configurable enclose tag/class, id prefix, optional classes,constraints, input type, place holder
  Output := TStringList.Create;
  try
    Output.Add('<form role="form">');
    for i := 0 to DataView.Fields.Count - 1 do
    begin
      Field := DataView.Fields[i];
      FieldHTMLId := Format('%s-%s', [LowerCase(Field.Model.Name), LowerCase(Field.FieldName)]);
      FieldHTML := Format(FieldHTMLTemplate, [FieldHTMLId, Field.DisplayLabel,
        LowerCase(Field.FieldName), FieldHTMLId, LowerCase(Field.FieldName)]);
      Output.Add(FieldHTML);
    end;
    Output.Add('</form>');
    Output.SaveToFile('export_htmlform.html');
  finally
    Output.Destroy;
  end;
end;

end.

