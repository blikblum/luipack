unit ContactView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, JSONFormMediator, AdvancedLabel, ContactPresenter,
  CategoryModel, SimpleJSONModel;

type

  { TContactForm }

  TContactForm = class(TForm)
    EditCategoriesLabel: TAdvancedLabel;
    CancelButton: TBitBtn;
    CategoryComboBox: TComboBox;
    ContactMediator: TJSONFormMediator;
    Label1: TLabel;
    NameEdit: TLabeledEdit;
    SaveButton: TBitBtn;
    procedure EditCategoriesLabelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FPresenter: TContactPresenter;
    procedure LoadCategories;
    procedure SetPresenter(Value: TContactPresenter);
  public
  published
    property Presenter: TContactPresenter read FPresenter write SetPresenter;
  end;

var
  ContactForm: TContactForm;

implementation

{$R *.lfm}

{ TContactForm }

procedure TContactForm.FormShow(Sender: TObject);
begin
  LoadCategories;
  ContactMediator.Data := FPresenter.Contact.Data;
  ContactMediator.LoadData;
end;

procedure TContactForm.EditCategoriesLabelClick(Sender: TObject);
begin
  if FPresenter.EditCategories then
    LoadCategories;
end;

procedure TContactForm.SaveButtonClick(Sender: TObject);
begin
  ContactMediator.SaveData;
  FPresenter.SaveContact;
end;

procedure TContactForm.SetPresenter(Value: TContactPresenter);
begin
  FPresenter := Value;
end;

procedure TContactForm.LoadCategories;
var
  Categories: TSimpleJSONCollection;
begin
  Categories := TSimpleJSONCollection.Create(TCategory);
  try
    //categories is cached so do not worry about latency/traffic issues
    Categories.Fetch;
    ContactMediator.ElementByName('categoryid').OptionsData.Elements['items'] := Categories.Data.Clone;
    ContactMediator.ElementByName('categoryid').Reset(True);
  finally
    Categories.Free;
  end;
end;

end.

