unit SearchEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LMessages, LCLProc, Graphics;

type

  TSearchEditOption = (
    seoExecuteEmpty,
    seoExecuteOnEditDone
  );
  
  TSearchEditOptions = set of TSearchEditOption;
  
  { TSearchEdit }

  TSearchEdit = class (TCustomEdit)
  private
    FIsEmpty: Boolean;
    FEmptyText: String;
    FOnExecute: TNotifyEvent;
    FOptions: TSearchEditOptions;
    procedure SetEmptyText(const AValue: String);
    procedure SetOptions(const AValue: TSearchEditOptions);
  protected
    procedure EditingDone; override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure TextChanged; override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    procedure Execute;
  published
    property EmptyText: String read FEmptyText write SetEmptyText;
    property IsEmpty: Boolean read FIsEmpty;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property Options: TSearchEditOptions read FOptions write SetOptions;
  end;

implementation

{ TSearchEdit }

procedure TSearchEdit.SetEmptyText(const AValue: String);
begin
  if FEmptyText=AValue then exit;
  FEmptyText:=AValue;
end;

procedure TSearchEdit.SetOptions(const AValue: TSearchEditOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TSearchEdit.EditingDone;
begin
  inherited EditingDone;
  if seoExecuteOnEditDone in FOptions then
    Execute;
end;

function TSearchEdit.RealGetText: TCaption;
var
  VisibleText: String;
begin
  VisibleText := inherited RealGetText;
  if FIsEmpty and (VisibleText = FEmptyText) then
    Result := ''
  else
    Result := VisibleText;
end;

procedure TSearchEdit.RealSetText(const Value: TCaption);
begin
  DebugLn('RealSetText: ',Value);
end;

procedure TSearchEdit.TextChanged;
begin
  DebugLn('TextChanged: ',Text);
  DebugLn('FIsEmpty: ',BoolToStr(FIsEmpty, True));
  FIsEmpty := Trim(RealGetText) = '';
  inherited TextChanged;
end;

procedure TSearchEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if FIsEmpty then
  begin
    Font.Color := clWindowText;
    inherited RealSetText('');
  end;
end;

procedure TSearchEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if FIsEmpty then
  begin
    Font.Color := clGray;
    inherited RealSetText(FEmptyText);
  end;
end;

procedure TSearchEdit.Execute;
begin
  if (FOnExecute <> nil) and (not FIsEmpty or (seoExecuteEmpty in FOptions)) then
    FOnExecute(Self);
end;

end.

