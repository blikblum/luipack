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

uses
  LuiDOMClasses;

{ THTMLDataViewExporter }

class function THTMLDataViewExporter.Description: String;
begin
  Result := 'HTML Form';
end;

class procedure THTMLDataViewExporter.Execute(DataView: TDataView);
var
  DOMTree: TTagTree;
  Field: TDataViewField;
  FieldId: String;
  i: Integer;
begin
  //todo: use DOMTree
  //todo: implement options: configurable enclose tag/class, id prefix, optional classes,constraints, input type, place holder
  DOMTree := TTagTree.Create;
  try
    DOMTree.Open('form').SetAttribute('role', 'form');
    for i := 0 to DataView.Fields.Count - 1 do
    begin
      Field := DataView.Fields[i];
      FieldId := Format('%s-%s', [LowerCase(Field.Model.Name), LowerCase(Field.FieldName)]);
      case Field.FieldType of
        dftBoolean:
          begin
            DOMTree
            .Open('div').SetAttribute('class', 'checkbox')
              .Open('label')
                .Add('input')
                  .SetAttribute('type', 'checkbox')
                  .SetAttribute('name', LowerCase(Field.FieldName))
                .AddText(Field.DisplayLabel)
              .Close
            .Close;
          end;
      else
        begin
          DOMTree
          .Open('div').SetAttribute('class', 'form-group')
            .Add('label', Field.DisplayLabel).SetAttribute('for', FieldId)
            .Add('input')
              .SetAttribute('type', 'text')
              .SetAttribute('class', 'form-control')
              .SetAttribute('name', LowerCase(Field.FieldName))
              .SetAttribute('id', FieldId)
          .Close;
        end;
      end;
    end;
    DOMTree.Close;
    DOMTree.SaveToFile('export_htmlform2.html');
  finally
    DOMTree.Destroy;
  end;
end;

end.

