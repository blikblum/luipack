unit LazReportExporterConfigView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, Buttons, DataView;

type

  { TLazReportExporterConfigForm }

  TLazReportExporterConfigForm = class(TForm)
    ExportButton: TBitBtn;
    CancelButton: TBitBtn;
    StyleComboBox: TComboBox;
    LabelPositionComboBox: TComboBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    FView: TDataView;
    procedure ExportReport(const TemplateFileName: String);
  public
    { public declarations }
    property View: TDataView read FView write FView;
  end;

var
  LazReportExporterConfigForm: TLazReportExporterConfigForm;

implementation

uses
  DOM, XMLRead, XPath;

{$R *.lfm}

{ TLazReportExporterConfigForm }

const
  BandTemplate = '<Object1>'+ LineEnding +
    '  <Name Value="MasterData1"/>'+ LineEnding +
    '  <ClassName Value="TfrBandView"/>'+ LineEnding +
    '  <Visible Value="True"/>'+ LineEnding +
    '  <Typ Value="gtBand"/>'+ LineEnding +
    '  <StreamMode Value="0"/>'+ LineEnding +
    '  <Size>'+ LineEnding +
    '  <Left Value="0"/>'+ LineEnding +
    '  <Top Value="172"/>'+ LineEnding +
    '  <Width Value="753"/>'+ LineEnding +
    '  <Height Value="144"/>'+ LineEnding +
    '  </Size>'+ LineEnding +
    '  <Flags Value="48"/>'+ LineEnding +
    '  <Data>'+ LineEnding +
    '  <Script Value=""/>'+ LineEnding +
    '  <GapX Value="0"/>'+ LineEnding +
    '  <GapY Value="0"/>'+ LineEnding +
    '  </Data>'+ LineEnding +
    '  <Tag Value=""/>'+ LineEnding +
    '  <FURLInfo Value=""/>'+ LineEnding +
    '  <Frames>'+ LineEnding +
    '  <Restrictions Value=""/>'+ LineEnding +
    '  </Frames>'+ LineEnding +
    '  <BandType Value="btMasterData"/>'+ LineEnding +
    '  <Condition Value=""/>'+ LineEnding +
    '  <DatasetStr Value="1"/>'+ LineEnding +
    '  <Child Value=""/>'+ LineEnding +
    '</Object1>';

procedure TLazReportExporterConfigForm.ExportReport(
  const TemplateFileName: String);
var
  TemplateDoc: TXMLDocument;
  PagePath, ObjectCountPath: TXPathVariable;
  PageNode, ObjectCountNode, BandNode: TDOMNode;
  ObjectCount, i: Integer;
begin
  try
    ReadXMLFile(TemplateDoc, TemplateFileName);
    PagePath := EvaluateXPathExpression('//Page1', TemplateDoc.DocumentElement);
    if PagePath.AsNodeSet.Count = 0 then
      ShowMessageFmt('Unable to find page in template file "%s"', [TemplateFileName]);
    PageNode := TObject(PagePath.AsNodeSet[0]) as TDOMNode;
    ObjectCountNode := PageNode.FindNode('ObjectCount');
    if ObjectCountNode = nil then
      ShowMessageFmt('Unable to find objectcount in template file "%s"', [TemplateFileName]);
    ObjectCount := StrToInt(ObjectCountNode.Attributes.GetNamedItem('Value').NodeValue);
    Inc(ObjectCount);
    //BandNode := TemplateDoc.CreateElement(Format('Object%d', [ObjectCount]));

    for i := 0 to View.Fields.Count - 1 do
    begin

    end;
  except
    ShowMessageFmt('Unable to load template file "%s"', [TemplateFileName]);
  end;
end;

end.

