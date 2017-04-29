program FireMonkeyFaceApi;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufmFireMonkeyMain in 'ufmFireMonkeyMain.pas' {Form1},
  uFaceApi.Base in 'Library\uFaceApi.Base.pas',
  uFaceApi.Content.Types in 'Library\uFaceApi.Content.Types.pas',
  uFaceApi.FaceAttributes in 'Library\uFaceApi.FaceAttributes.pas',
  uFaceApi.FaceDetectOptions in 'Library\uFaceApi.FaceDetectOptions.pas',
  uFaceApi in 'Library\uFaceApi.pas',
  uFaceApi.Servers.Types in 'Library\uFaceApi.Servers.Types.pas',
  uIFaceApi in 'Library\uIFaceApi.pas',
  uFunctions.StringHelper in 'Functions\uFunctions.StringHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
