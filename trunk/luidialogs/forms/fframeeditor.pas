unit fFrameEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, LMessages, LCLType, LuiLCLMessages, LuiLCLInterfaces, fpjson;

  { TFrameEditorForm }

type

  TFrameEditorForm = class(TForm, IFrameController)
    PrintButton: TBitBtn;
    CancelButton: TBitBtn;
    ButtonPanel: TPanel;
    SaveButton: TBitBtn;
    procedure ButtonClick(Sender: TObject);
  private
    FActionPrefix: String;
    FFrame: TCustomFrame;
    procedure ConfigureButtons(Data: TJSONData);
    { private declarations }
    //todo: remove msg
    procedure FrameMessage(Sender: TCustomFrame; const MessageId: String; Data: Variant);
    procedure CMChildDataChanged(var Msg: TLMessage); message CM_CHILDDATACHANGED;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; FrameClass: TCustomFrameClass;
      FrameProperties: array of const; const Options: String);
  end; 

implementation

{$R *.lfm}

uses
  LuiRTTIUtils, LuiMiscUtils, LuiJSONUtils, strutils, Variants;

{ TFrameEditorForm }

procedure TFrameEditorForm.ButtonClick(Sender: TObject);
var
  BitButton: TBitBtn absolute Sender;
begin
  if FFrame.Perform(CM_PARENTBUTTONPRESSED, 0, Integer(BitBtnKindToMsgDlgBtn[BitButton.Kind])) = 0 then
  begin
    case BitButton.Kind of
      bkOK: ModalResult := mrOK;
      bkCancel: ModalResult := mrCancel;
    end;
  end;
  if BitButton = SaveButton then
  begin
    CallMethod(FFrame, Format('%sSave', [FActionPrefix]));
    //todo: Remove
    CallMethod(FFrame, 'save');
  end
  else if BitButton = PrintButton then
  begin
    CallMethod(FFrame, Format('%sPrint', [FActionPrefix]));
  end
  else if BitButton = CancelButton then
  begin
    CallMethod(FFrame, Format('%sCancel', [FActionPrefix]));
  end;
end;

procedure TFrameEditorForm.ConfigureButtons(Data: TJSONData);
var
  ButtonsData: TJSONArray absolute Data;
  ButtonData: TJSONData;
  i: Integer;
begin
  if (Data = nil) or (Data.JSONType <> jtArray) then
    Exit;
  PrintButton.Visible := False;
  SaveButton.Visible := False;
  CancelButton.Visible := False;
  for i := 0 to ButtonsData.Count - 1 do
  begin
    ButtonData := ButtonsData.Items[i];
    case ButtonData.JSONType of
      jtString:
        begin
          case AnsiIndexText(ButtonData.AsString, ['save', 'cancel', 'print']) of
            0: SaveButton.Visible := True;
            1: CancelButton.Visible := True;
            2: PrintButton.Visible := True;
          end;
        end;
    end;
  end;
end;

procedure TFrameEditorForm.FrameMessage(Sender: TCustomFrame;
  const MessageId: String; Data: Variant);
var
  ButtonId: String;
begin
  ButtonId := VarToStr(Data);
  if MessageId = 'enable-action' then
  begin
    if ButtonId = 'save' then
      PrintButton.Enabled := True;
  end
  else if MessageId = 'disable-action' then
  begin
    if ButtonId = 'save' then
      PrintButton.Enabled := False;
  end
end;

procedure TFrameEditorForm.CMChildDataChanged(var Msg: TLMessage);
begin
  PrintButton.Enabled := Boolean(Msg.lParam);
end;

constructor TFrameEditorForm.Create(TheOwner: TComponent;
  FrameClass: TCustomFrameClass; FrameProperties: array of const;
  const Options: String);
var
  i: Integer;
  ACaption: String;
  OptionsData: TJSONObject;
begin
  inherited Create(TheOwner);
  FFrame := FrameClass.Create(Self);
  //set the properties
  SetObjectProperties(FFrame, FrameProperties);
  Height := FFrame.Height + ButtonPanel.Height;
  Width := FFrame.Width;
  FFrame.Align := alClient;
  FActionPrefix := 'Frame';
  Position := poOwnerFormCenter;
  if Options <> '' then
  begin
    if TryStrToJSON(Options, OptionsData) then
    begin
      ConfigureButtons(OptionsData.Find('buttons'));
      FActionPrefix := OptionsData.Get('actionprefix', FActionPrefix);
      OptionsData.Free;
    end
    else
    begin
      //todo: remove this
      i := Pos(';', Options);
      if i > 0 then
      begin
        ACaption := Copy(Options, 1, i -1);
        if ACaption <> '' then
          PrintButton.Caption := ACaption;
        ACaption := Copy(Options, i + 1, Length(Options) - i);
        if ACaption <> '' then
          CancelButton.Caption := ACaption;
      end;
    end;
  end;
  FFrame.Parent := Self;
  Caption := FFrame.Caption;
  SetObjectProperties(FFrame, ['FrameController', Self as IFrameController]);
  //todo: remove InitControl. Keep FrameLoad that is consistent with other components
  //todo: add parameter to choose what method to call
  CallMethod(FFrame, 'InitControl');
  CallMethod(FFrame, Format('%sLoad', [FActionPrefix]));
  FFrame.Visible := True;
end;

initialization


end.


