program FaceAPI;

uses
  Vcl.Forms,
  ufmMain in 'ufmMain.pas' {Form1},
  uFaceApi in 'uFaceApi.pas',
  uFunctions.StringHelper in 'Functions\uFunctions.StringHelper.pas',
  uFaceApi.Servers.Types in 'uFaceApi.Servers.Types.pas',
  uFaceApi.Content.Types in 'uFaceApi.Content.Types.pas',
  uFaceApi.Base in 'uFaceApi.Base.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
