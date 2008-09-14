unit xpStyleLuiBarSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, Graphics, Controls, LResources;

type

  { TxpStyleLuiBarSample }

  TxpStyleLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    FImages: TImageList;
    procedure GetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
    procedure Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
      DrawType: TLuiBarDrawType; var Allowed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TxpStyleLuiBarSample }

procedure TxpStyleLuiBarSample.GetImageInfo(Sender: TLuiBar;
  Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
begin
  ImageInfo.Index := Cell.Index;
  //ImageInfo.Effect := ;
end;

procedure TxpStyleLuiBarSample.Drawing(Sender: TLuiBar; Cell: TLuiBarCell;
  DrawType: TLuiBarDrawType; var Allowed: Boolean);
begin
  Allowed := (DrawType <> dtCell) or (Cell.Index = Sender.SelectedIndex);
end;

constructor TxpStyleLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clRed;
  FImages := TImageList.Create(AOwner);
  with FImages do
  begin
    Width := 32;
    Height := 32;
    AddLazarusResource('xpStyle0', clDefault);
    AddLazarusResource('xpStyle1', clDefault);
    AddLazarusResource('xpStyle2', clDefault);
    AddLazarusResource('xpStyle3', clDefault);
  end;
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    Parent := Self;
    SetBounds(10, 10, 400, 400);
    Options := [lboEmulateTab, lboOutLineClientArea, lboCenterImage];
    ImagePosition := ipTop;
    Position := poLeft;
    OnDrawing := @Drawing;
    Images := FImages;
    OnGetImageInfo := @GetImageInfo;
    CellWidth := 80;
    CellHeight := 60;
    CellRoundRadius := 6;
    with Colors do
    begin
      Background := clRed;
      Text := clWhite;
      Selected := clWhite;
      SelectedText := clBlue;
      ClientArea := clWhite;
      OutLine := clGray;
    end;
    Spacing := 5;
    OuterOffset := 4;
    OutLineWidth := 1;
    Cells.Add('Summary');
    Cells.Add('Features');
    Cells.Add('Events');
    Cells.Add('Description');
    Visible := True;
  end;
end;

initialization
  {$I xpstyleimages.lrs}

end.

