unit LazReportExporterConfigView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, Buttons, DataView;

type

  { TLazReportExporterConfigForm }

  TLazReportExporterConfigForm = class(TForm)
    ExportButton: TBitBtn;
    CancelButton: TBitBtn;
    StyleComboBox: TComboBox;
    LabelPositionComboBox: TComboBox;
    TemplateFileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ExportButtonClick(Sender: TObject);
    procedure TemplateFileNameEditAcceptFileName(Sender: TObject;
      var Value: String);
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
  DOM, XMLRead, XMLWrite, XPath;

{$R *.lfm}

{ TLazReportExporterConfigForm }

const
  BandTopOffset = 200;
  BandWidthLimit = 700;
  BandTemplate = '<Object%d>'+ LineEnding +
  '  <Name Value="ExportedBand"/>'+ LineEnding +
  '  <ClassName Value="TfrBandView"/>'+ LineEnding +
  '  <Visible Value="True"/>'+ LineEnding +
  '  <Typ Value="gtBand"/>'+ LineEnding +
  '  <StreamMode Value="0"/>'+ LineEnding +
  '  <Size>'+ LineEnding +
  '    <Left Value="0"/>'+ LineEnding +
  '    <Top Value="200"/>'+ LineEnding +
  '    <Width Value="700"/>'+ LineEnding +
  '    <Height Value="400"/>'+ LineEnding +
  '  </Size>'+ LineEnding +
  '  <Flags Value="48"/>'+ LineEnding +
  '  <Data>'+ LineEnding +
  '    <Script Value=""/>'+ LineEnding +
  '    <GapX Value="0"/>'+ LineEnding +
  '    <GapY Value="0"/>'+ LineEnding +
  '  </Data>'+ LineEnding +
  '  <Tag Value=""/>'+ LineEnding +
  '  <FURLInfo Value=""/>'+ LineEnding +
  '  <Frames>'+ LineEnding +
  '    <Restrictions Value=""/>'+ LineEnding +
  '  </Frames>'+ LineEnding +
  '  <BandType Value="btMasterData"/>'+ LineEnding +
  '  <Condition Value=""/>'+ LineEnding +
  '  <DatasetStr Value="1"/>'+ LineEnding +
  '  <Child Value=""/>'+ LineEnding +
  '</Object%0:d>';



  MemoTemplate = '<Object%d>'+ LineEnding +
  '  <Name Value="Memo%0:d"/>'+ LineEnding +
  '  <ClassName Value="TfrMemoView"/>'+ LineEnding +
  '  <Visible Value="True"/>'+ LineEnding +
  '  <Typ Value="gtMemo"/>'+ LineEnding +
  '  <StreamMode Value="0"/>'+ LineEnding +
  '  <Size>'+ LineEnding +
  '    <Left Value="%d"/>'+ LineEnding +
  '    <Top Value="%d"/>'+ LineEnding +
  '    <Width Value="%d"/>'+ LineEnding +
  '    <Height Value="%d"/>'+ LineEnding +
  '  </Size>'+ LineEnding +
  '  <Flags Value="3"/>'+ LineEnding +
  '  <FillColor Value="clNone"/>'+ LineEnding +
  '  <Frames>'+ LineEnding +
  '    <FrameColor Value="clBlack"/>'+ LineEnding +
  '    <FrameStyle Value="frsSolid"/>'+ LineEnding +
  '    <FrameWidth Value="1"/>'+ LineEnding +
  '    <FrameBorders Value=""/>'+ LineEnding +
  '    <Restrictions Value=""/>'+ LineEnding +
  '  </Frames>'+ LineEnding +
  '  <Data>'+ LineEnding +
  '    <Format Value="556"/>'+ LineEnding +
  '    <FormatStr Value=""/>'+ LineEnding +
  '    <Memo Value="%s&#xD;&#xA;"/>'+ LineEnding +
  '    <Script Value=""/>'+ LineEnding +
  '    <GapX Value="0"/>'+ LineEnding +
  '    <GapY Value="0"/>'+ LineEnding +
  '    <OnClick Value=""/>'+ LineEnding +
  '    <OnMouseEnter Value=""/>'+ LineEnding +
  '    <OnMouseLeave Value=""/>'+ LineEnding +
  '    <DetailReport Value=""/>'+ LineEnding +
  '    <ParagraphGap Value="0"/>'+ LineEnding +
  '    <LineSpacing Value="2"/>'+ LineEnding +
  '  </Data>'+ LineEnding +
  '  <Tag Value=""/>'+ LineEnding +
  '  <FURLInfo Value=""/>'+ LineEnding +
  '  <Font>'+ LineEnding +
  '    <Name Value="Arial"/>'+ LineEnding +
  '    <Size Value="%d"/>'+ LineEnding +
  '    <Color Value="clBlack"/>'+ LineEnding +
  '    <Charset Value="0"/>'+ LineEnding +
  '    <Style Value=""/>'+ LineEnding +
  '  </Font>'+ LineEnding +
  '  <Highlight>'+ LineEnding +
  '    <FontStyle Value="2"/>'+ LineEnding +
  '    <FontColor Value="clBlack"/>'+ LineEnding +
  '    <FillColor Value="clWhite"/>'+ LineEnding +
  '    <HighlightStr Value=""/>'+ LineEnding +
  '  </Highlight>'+ LineEnding +
  '  <Alignment Value="taLeftJustify"/>'+ LineEnding +
  '  <Layout Value="tlTop"/>'+ LineEnding +
  '  <Angle Value="0"/>'+ LineEnding +
  '  <Justify Value="False"/>'+ LineEnding +
  '  <Cursor Value="0"/>'+ LineEnding +
  '</Object%0:d>';


procedure LoadFragment(ParentNode: TDOMNode; const Fragment: String);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Fragment);
  try
    ReadXMLFragment(ParentNode, Stream);
  finally
    Stream.Destroy;
  end;
end;

procedure AddFields(ParentNode: TDOMNode; Fields: TDataViewFields; var ObjectCount: Integer);
var
  i, Left, Top, Width, Height, FontSize: Integer;
  Field: TDataViewField;
  Fragment: String;
begin
  Left := 0;
  Top := BandTopOffset;
  Height := 20;
  Width := 80;
  FontSize := 10;
  for i := 0 to Fields.Count - 1 do
  begin
    Field := Fields.Items[i];

    //label (boxed - inside)
    Inc(ObjectCount);
    Fragment := Format(MemoTemplate, [ObjectCount, Left, Top, Width, Height - 5,
      Field.DisplayLabel, FontSize - 2]);
    LoadFragment(ParentNode, Fragment);

    //fieldname
    Inc(ObjectCount);
    Fragment := Format(MemoTemplate, [ObjectCount, Left, Top + Height - 5, Width, Height,
      '[' + Field.FieldName +']', FontSize]);
    LoadFragment(ParentNode, Fragment);

    Inc(Left, Width);
    if (Left + Width) > BandWidthLimit then
    begin
      Left := 0;
      Inc(Top, (Height * 2) - 5 + 2);
    end;
  end;
end;

procedure TLazReportExporterConfigForm.ExportButtonClick(Sender: TObject);
begin
  ExportReport(TemplateFileNameEdit.Text);
end;

procedure TLazReportExporterConfigForm.TemplateFileNameEditAcceptFileName(
  Sender: TObject; var Value: String);
begin
  ExportButton.Enabled := FileExistsUTF8(Value);
end;

procedure TLazReportExporterConfigForm.ExportReport(
  const TemplateFileName: String);
var
  TemplateDoc: TXMLDocument;
  PagePath: TXPathVariable;
  PageNode, ObjectCountNode: TDOMNode;
  ObjectCount: Integer;
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
    LoadFragment(PageNode, Format(BandTemplate, [ObjectCount]));

    AddFields(PageNode, View.Fields, ObjectCount);

    ObjectCountNode.Attributes.GetNamedItem('Value').NodeValue := IntToStr(ObjectCount);

    WriteXMLFile(TemplateDoc, 'c:\temp\datahublazreporttest.lrf');
  except
    ShowMessageFmt('Unable to load template file "%s"', [TemplateFileName]);
  end;
end;

end.

