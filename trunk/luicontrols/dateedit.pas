unit DateEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbCtrls, LMessages, Graphics, Dialogs;

type

  TRecoverMode = (rmNone, rmClear, rmRestore);

  TNewValueEvent = procedure(Sender: TObject; const NewText: String; IsValid: Boolean) of object;

  TDBDateMaskEditOption = (deoNullDateAsError);

  TDBDateMaskEditOptions = set of TDBDateMaskEditOption;

  { TDBDateMaskEdit }

  TDBDateMaskEdit = class(TDBEdit)
  private
    FErrorColor: TColor;
    FDataLink: TFieldDataLink;
    FErrorMessage: String;
    FOldDataChange: TNotifyEvent;
    FOnNewValue: TNewValueEvent;
    FOptions: TDBDateMaskEditOptions;
    FRecoverMode: TRecoverMode;
    FValidDate: Boolean;
    FErrorHandled: Boolean;
    FHandlingError: Boolean;
    procedure BuildEditMask;
    procedure DataChange(Sender: TObject);
    procedure ResetDataLink;
    procedure UpdateControlState(DateIsValid: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure UpdateText(const NewText: String);
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
  protected
    procedure CreateWnd; override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ValidDate: Boolean read FValidDate;
  published
    property ErrorColor: TColor read FErrorColor write FErrorColor default clWindow;
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
    property Options: TDBDateMaskEditOptions read FOptions write FOptions default [];
    property RecoverMode: TRecoverMode read FRecoverMode write FRecoverMode default rmNone;
    //events
    property OnNewValue: TNewValueEvent read FOnNewValue write FOnNewValue;
  end;

implementation

uses
  strutils;

{ TDBDateMaskEdit }

procedure TDBDateMaskEdit.BuildEditMask;
var
  S: String;
  i, FieldCount: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  S := '';
  FieldCount := 0;
  for i := 1 to Length(ShortDateFormat) do
  begin
    if ShortDateFormat[i] in ['M', 'm', 'D', 'd', 'Y', 'y'] then
    begin
      S := S + '9';
      Inc(FieldCount);
    end
    else
    begin
      //add an extra character to avoid date fields with only one character
      if FieldCount = 1 then
        S := S + '9';
      FieldCount := 0;
      S := S + DateSeparator;
    end;
  end;
  //the last field has only one character
  if FieldCount = 1 then
    S := S + '9';
  EditMask := '!' + S + ';1;_';
end;

procedure TDBDateMaskEdit.DataChange(Sender: TObject);
begin
  UpdateControlState((FDataLink.Field = nil) or (FDataLink.Field.Value <> Null) or
    not (deoNullDateAsError in FOptions));
  FOldDataChange(Sender);
end;

procedure TDBDateMaskEdit.ResetDataLink;
begin
  FDataLink.OnDataChange := nil;
  try
    FDataLink.Reset;
  finally
    FDataLink.OnDataChange := @DataChange;
  end;
end;

procedure TDBDateMaskEdit.UpdateControlState(DateIsValid: Boolean);
begin
  FValidDate := DateIsValid;
  if FValidDate then
    Color := clWindow
  else
    Color := FErrorColor;
end;

procedure TDBDateMaskEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
  S: String;
begin
  S := Text;
  FValidDate := TryStrToDate(S, D);
  if Assigned(FOnNewValue) then
    FOnNewValue(Self, S, FValidDate);
  if FValidDate then
  begin
    FDataLink.Field.Text := S;
    Color := clWindow;
  end
  else
  begin
    FHandlingError := True;
    FErrorHandled := True;
    if FErrorMessage <> '' then
      ShowMessage(AnsiReplaceText(FErrorMessage, '$(NewValue)', S));
    case FRecoverMode of
      rmNone: Color := FErrorColor;
      rmClear:
        begin
          UpdateText('');
          UpdateControlState(not (deoNullDateAsError in FOptions));
        end;
      rmRestore: FDataLink.Reset;
    end;
    FHandlingError := False;
  end;
end;

procedure TDBDateMaskEdit.UpdateText(const NewText: String);
begin
  //avoid calling the UpdateData event
  FDataLink.OnUpdateData := nil;
  try
    Text := NewText;
  finally
    FDataLink.OnUpdateData := @UpdateData;
  end;
end;

procedure TDBDateMaskEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  PushMask(True);
  //reset FDatalink to allow the user exit the control after the first message
  //maybe this can be removed when Pop/PushMask is incorporated in TDBEdit
  if not FValidDate and FErrorHandled then
    ResetDataLink;
  inherited;
end;

procedure TDBDateMaskEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  if not FValidDate and not FHandlingError then
    FErrorHandled := False;
  PopMask(True);
  inherited;
end;

procedure TDBDateMaskEdit.CreateWnd;
var
  OldText: String;
begin
  OldText := Text;
  //post pone mask build the maximum
  BuildEditMask;
  PushMask(True);
  //Setting EditMask clears the text. Update again here
  Text := OldText;
  inherited CreateWnd;
end;

procedure TDBDateMaskEdit.KeyPress(var Key: char);
var
  OldText: String;
begin
  OldText := RealGetText;
  inherited KeyPress(Key);
  if OldText <> RealGetText then
    FErrorHandled := False;
end;

constructor TDBDateMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  FOldDataChange := FDataLink.OnDataChange;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FRecoverMode := rmNone;
  FErrorColor := clWindow;
end;

end.

