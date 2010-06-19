unit fFrameEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, LMessages, LCLType, LuiMessages;

  { TFrameEditorForm }

type

  TFrameEditorForm = class(TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    ButtonPanel: TPanel;
    procedure ButtonClick(Sender: TObject);
  private
    FFrame: TCustomFrame;
    { private declarations }
    procedure CMChildDataChanged(var Msg: TLMessage); message CM_CHILDDATACHANGED;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; FrameClass: TCustomFrameClass;
      FrameProperties: array of const; ButtonCaptions: String);
  end; 

implementation

{$R *.lfm}

uses
  LuiRTTIUtils, LuiMiscUtils;

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
  CallMethod(FFrame, 'InitControl');
  FFrame.Visible := True;
end;

initialization


end.

