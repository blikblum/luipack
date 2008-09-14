program MegaDemo;

uses
  Interfaces,
  Forms,
  Umegademo in 'Umegademo.pas' {MainForm};


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
