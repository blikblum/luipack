unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    LeftPanel: TPanel;
    SamplesNotebook: TNotebook;
    SamplesList: TListBox;
    SplitterVertical: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure SamplesListSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    procedure RegisterSample(const AName: String; APage: TPage);
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  FolhaSample, YahooSample, UolSample, MSSDKSample, UbuntuSample,
  ConversaAfiadaSample, YouTubeSample, LgMobileLuiBarSample, xpStyleLuiBarSample,
  TerraEsportesLuiBarSample, SqliteLuiBarSample, UTorrentLuiBarSample,
  GloboLuiBarSample, GnomeDoLuiBarSample, MediaControlLuiBarSample, WidgetSample;

{ TFormMain }

procedure TFormMain.SamplesListSelectionChange(Sender: TObject; User: boolean);
begin
  if SamplesList.ItemIndex = -1 then
    Exit;
  SamplesNotebook.ActivePageComponent := TPage(SamplesList.Items.Objects[SamplesList.ItemIndex]);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  RegisterSample('Folha', TFolhaLuiBarSample.Create(Self));
  RegisterSample('Yahoo', TYahooLuiBarSample.Create(Self));
  RegisterSample('Uol', TUolLuiBarSample.Create(Self));
  RegisterSample('MSSDK', TMSSDKLuiBarSample.Create(Self));
  RegisterSample('Ubuntu', TUbuntuLuiBarSample.Create(Self));
  RegisterSample('Conversa Afiada', TConversaAfiadaLuiBarSample.Create(Self));
  RegisterSample('YouTube', TYouTubeLuiBarSample.Create(Self));
  RegisterSample('LG Mobile', TLgMobileLuiBarSample.Create(Self));
  RegisterSample('xpStyle', TxpStyleLuiBarSample.Create(Self));
  RegisterSample('Terra Esportes', TTerraEsportesLuiBarSample.Create(Self));
  RegisterSample('Sqlite', TSqliteLuiBarSample.Create(Self));
  RegisterSample('UTorrent', TUTorrentLuiBarSample.Create(Self));
  RegisterSample('Globo', TGloboLuiBarSample.Create(Self));
  RegisterSample('GnomeDo', TGnomeDoLuiBarSample.Create(Self));
  RegisterSample('Media Control (aTunes)', TMediaControlLuiBarSample.Create(Self));
  RegisterSample('Lui Widgets', TWidgetLuiBarSample.Create(Self));
end;

procedure TFormMain.RegisterSample(const AName: String; APage: TPage);
begin
  APage.Parent := SamplesNotebook;
  {$if defined(LCLGtk) or defined(LCLWin32)}
  APage.TabVisible := False;
  {$endif}
  SamplesList.Items.AddObject(AName, APage);
end;

initialization
  {$I fmain.lrs}

end.

