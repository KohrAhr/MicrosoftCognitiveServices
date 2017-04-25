program FaceAPI;

uses
  Vcl.Forms,
  ufmMain in 'ufmMain.pas' {Form1},
  uFaceApi in 'uFaceApi.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
