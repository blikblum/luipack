unit SearchEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LMessages;

type

  { TSearchEdit }

  TSearchEdit = class (TCustomEdit)
  private
    FIsEmpty: Boolean;
    FEmptyText: String;
    procedure SetEmptyText(const AValue: String);
  protected
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
  published
    property EmptyText: String read FEmptyText write SetEmptyText;
    property IsEmpty: Boolean read FIsEmpty;

  end;

implementation

{ TSearchEdit }

procedure TSearchEdit.SetEmptyText(const AValue: String);
begin
  if FEmptyText=AValue then exit;
  FEmptyText:=AValue;
end;

procedure TSearchEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
end;

procedure TSearchEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
end;

end.

