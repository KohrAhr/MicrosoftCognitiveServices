program FaceAPI;

uses
  Vcl.Forms,
  ufmMain in 'ufmMain.pas' {Form1},
  uFunctions.StringHelper in 'Functions\uFunctions.StringHelper.pas',
  uFaceApi.Base in 'Library\uFaceApi.Base.pas',
  uFaceApi.Content.Types in 'Library\uFaceApi.Content.Types.pas',
  uFaceApi in 'Library\uFaceApi.pas',
  uFaceApi.Servers.Types in 'Library\uFaceApi.Servers.Types.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
