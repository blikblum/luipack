unit fTagsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fDatasetEditor, FileUtil, Forms, Controls, Graphics,
  Dialogs, Buttons;

type

  { TTagsEditorForm }

  TTagsEditorForm = class(TForm)
    CloseButton: TBitBtn;
    DatasetEditorFrame1: TDatasetEditorFrame;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TagsEditorForm: TTagsEditorForm;

implementation

{$R *.lfm}

end.

