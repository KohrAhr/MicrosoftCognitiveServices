program FaceAPI;

uses
  Vcl.Forms,
  ufmMain in 'ufmMain.pas' {fmMain},
  uFunctions.StringHelper in 'Functions\uFunctions.StringHelper.pas',
  uFaceApi.Base in 'Library\uFaceApi.Base.pas',
  uFaceApi.Content.Types in 'Library\uFaceApi.Content.Types.pas',
  uFaceApi.Core in 'Library\uFaceApi.Core.pas',
  uFaceApi.Servers.Types in 'Library\uFaceApi.Servers.Types.pas',
  uFaceApi.FaceAttributes in 'Library\uFaceApi.FaceAttributes.pas',
  uFaceApi.FaceDetectOptions in 'Library\uFaceApi.FaceDetectOptions.pas',
  uFunctions.InetHelper in 'Functions\uFunctions.InetHelper.pas',
  uFunctions.FaceApiHelper in 'Functions\uFunctions.FaceApiHelper.pas',
  uFaceApi.ServersAccess.Types in 'Library\uFaceApi.ServersAccess.Types.pas',
  uFaceApi.Core.PersonGroup in 'Library\uFaceApi.Core.PersonGroup.pas',
  uIFaceApi.PersonGroup in 'Library\uIFaceApi.PersonGroup.pas',
  uFaceApi.Core.Face in 'Library\uFaceApi.Core.Face.pas',
  uIFaceApi.Face in 'Library\uIFaceApi.Face.pas',
  uIFaceApi.Base in 'Library\uIFaceApi.Base.pas',
  uIFaceApi.Core in 'Library\uIFaceApi.Core.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
