unit fFrameEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, LMessages, LCLType, LuiLCLMessages, LuiLCLInterfaces;

  { TFrameEditorForm }

type

  TFrameEditorForm = class(TForm, IFrameController)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    ButtonPanel: TPanel;
    procedure ButtonClick(Sender: TObject);
  private
    FFrame: TCustomFrame;
    { private declarations }
    //todo: remove msg
    procedure FrameMessage(Sender: TCustomFrame; const MessageId: String; Data: Variant);
    procedure CMChildDataChanged(var Msg: TLMessage); message CM_CHILDDATACHANGED;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; FrameClass: TCustomFrameClass;
      FrameProperties: array of const; ButtonCaptions: String);
  end; 

implementation

{$R *.lfm}

uses
  LuiRTTIUtils, LuiMiscUtils, Variants;

{ TFrameEditorForm }

procedure TFrameEditorForm.ButtonClick(Sender: TObject);
var
  BitButton: TBitBtn absolute Sender;
begin
  if FFrame.Perform(CM_PARENTBUTTONPRESSED, 0, Integer(BitBtnKindToMsgDlgBtn[BitButton.Kind])) = 0 then
  begin
    if BitButton.Kind = bkOK then
      ModalResult := mrOK
    else
      ModalResult := mrCancel;
  end;
  if BitButton = OKButton then
    CallMethod(FFrame, 'save');
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
      OKButton.Enabled := True;
  end
  else if MessageId = 'disable-action' then
  begin
    if ButtonId = 'save' then
      OKButton.Enabled := False;
  end
end;

procedure TFrameEditorForm.CMChildDataChanged(var Msg: TLMessage);
begin
  OKButton.Enabled := Boolean(Msg.lParam);
end;

constructor TFrameEditorForm.Create(TheOwner: TComponent; FrameClass: TCustomFrameClass;
  FrameProperties: array of const; ButtonCaptions: String);
var
  i: Integer;
  ACaption: String;
begin
  inherited Create(TheOwner);
  FFrame := FrameClass.Create(Self);
  //set the properties
  SetObjectProperties(FFrame, FrameProperties);
  Height := FFrame.Height + ButtonPanel.Height;
  Width := FFrame.Width;
  FFrame.Align := alClient;
  Position := poOwnerFormCenter;
  if ButtonCaptions <> '' then
  begin
    i := Pos(';', ButtonCaptions);
    if i > 0 then
    begin
      ACaption := Copy(ButtonCaptions, 1, i -1);
      if ACaption <> '' then
        OKButton.Caption := ACaption;
      ACaption := Copy(ButtonCaptions, i + 1, Length(ButtonCaptions) - i);
      if ACaption <> '' then
        CancelButton.Caption := ACaption;
    end;
  end;
  FFrame.Parent := Self;
  Caption := FFrame.Caption;
  SetObjectProperties(FFrame, ['FrameController', Self as IFrameController]);
  //todo: remove InitControl. Keep FrameLoad that is consistent with other components
  //todo: add parameter to choose what method to call
  CallMethod(FFrame, 'InitControl');
  CallMethod(FFrame, 'FrameLoad');
  FFrame.Visible := True;
end;

initialization


end.

