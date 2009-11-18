unit UolSample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LuiBar, Graphics, Controls, LResources;

type

  { TUolLuiBarSample }

  TUolLuiBarSample = class(TPage)
  private
    FBar: TLuiBar;
    FBar2: TLuiBar;
    FBar3: TLuiBar;
    FBar4: TLuiBar;
    ImgList: TImageList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
  end;

implementation

{ TUolLuiBarSample }

constructor TUolLuiBarSample.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImgList := TImageList.Create(Self);
  with ImgList do
  begin
    AddLazarusResource('info', clDefault);
    AddLazarusResource('error', clDefault);
    AddLazarusResource('warning', clDefault);
    AddLazarusResource('value', clDefault);
    AddLazarusResource('entermethod', clDefault);
    AddLazarusResource('exitmethod', clDefault);
  end;
  Color := RGBToColor(183, 183, 183);
  FBar := TLuiBar.Create(Self);
  with FBar do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 10, 400, 60);
    Options := [lboEmulateTab, lboHoverAsSelected, lboOutLineClientArea,
      lboVariableCellWidth, lboHotTrack];
    OutLineWidth := 1;
    Spacing := 4;

    //CellRoundRadius := 4;
    with Colors do
    begin
      Normal := RGBToColor(183, 183, 183);
      Background := Normal;
      Selected := RGBToColor(204, 204, 204);
      Hover := Selected;
      ClientArea := Selected;
      SelectedText := clBlack;
      Text := clBlack;
      OutLine := RGBToColor(227,227,227);
    end;
    Cells.Add('Web');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add('Imagens');
    OnGetImageInfo := @GetImageInfo;
    Images := ImgList;
    EndUpdate;
    Visible := True;
  end;

  FBar2 := TLuiBar.Create(Self);
  with FBar2 do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 100, 180, 200);
    Options := [lboEmulateTab, lboHoverAsSelected, lboOutLineClientArea, lboHotTrack, lboCenterImage];
    OutLineWidth := 1;
    Position := poLeft;
    CellAlign := caCenter;
    Images := ImgList;
    OnGetImageInfo := @GetImageInfo;
    Spacing := 4;
    //CellRoundRadius := 4;
    CellWidth := 100;
    with Colors do
    begin
      Normal := RGBToColor(183, 183, 183);
      Background := Normal;
      Selected := RGBToColor(204, 204, 204);
      Hover := Selected;
      ClientArea := Selected;
      SelectedText := clBlack;
      Text := clBlack;
      OutLine := RGBToColor(227,227,227);
    end;
    Cells.Add('Web');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add('Imagens');
    EndUpdate;
    Visible := True;
  end;
  
  FBar3 := TLuiBar.Create(Self);
  with FBar3 do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(220, 100, 200, 200);
    Options := [lboEmulateTab, lboHoverAsSelected, lboOutLineClientArea, lboHotTrack, lboCenterImage];
    OutLineWidth := 1;
    Position := poRight;
    Images := ImgList;
    ImagePosition := ipRight;
    OnGetImageInfo := @GetImageInfo;
    Spacing := 4;
    CellAlign := caInvert;
    //CellRoundRadius := 4;
    InitialSpace := 6;
    OuterOffset := 0;
    with Colors do
    begin
      Normal := RGBToColor(183, 183, 183);
      Background := Normal;
      Selected := RGBToColor(204, 204, 204);
      Hover := Selected;
      ClientArea := Selected;
      SelectedText := clBlack;
      Text := clBlack;
      OutLine := RGBToColor(227,227,227);
    end;
    Cells.Add('Web');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add('Imagens');
    EndUpdate;
    Visible := True;
  end;
  
  FBar4 := TLuiBar.Create(Self);
  with FBar4 do
  begin
    BeginUpdate;
    Parent := Self;
    SetBounds(10, 320, 400, 60);
    Options := [lboEmulateTab, lboHoverAsSelected, lboOutLineClientArea, lboHotTrack];
    OutLineWidth := 1;
    Position := poBottom;
    Spacing := 4;
    //CellRoundRadius := 4;
    with Colors do
    begin
      Normal := RGBToColor(183, 183, 183);
      Background := Normal;
      Selected := RGBToColor(204, 204, 204);
      Hover := Selected;
      ClientArea := Selected;
      SelectedText := clBlack;
      Text := clBlack;
      OutLine := RGBToColor(227,227,227);
    end;
    Cells.Add('Web');
    Cells.Add(UTF8Encode('Notícias'));
    Cells.Add('Imagens');
    EndUpdate;
    Visible := True;
  end;
end;

procedure TUolLuiBarSample.GetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell;
  var ImageInfo: TLuiBarImageInfo);
begin
  if Cell.Index <> 1 then
    ImageInfo.Index := Cell.Index + 1;
end;

initialization
  {$i logimages.lrs}

end.

