program FireMonkeyFaceApi;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufmFireMonkeyMain in 'ufmFireMonkeyMain.pas' {fmMain},
  uFaceApi.Base in 'Library\uFaceApi.Base.pas',
  uFaceApi.Content.Types in 'Library\uFaceApi.Content.Types.pas',
  uFaceApi.FaceAttributes in 'Library\uFaceApi.FaceAttributes.pas',
  uFaceApi.FaceDetectOptions in 'Library\uFaceApi.FaceDetectOptions.pas',
  uFaceApi.Core in 'Library\uFaceApi.Core.pas',
  uFaceApi.Servers.Types in 'Library\uFaceApi.Servers.Types.pas',
  uIFaceApi in 'Library\uIFaceApi.pas',
  uFunctions.FaceApiHelper in 'Functions\uFunctions.FaceApiHelper.pas',
  uFunctions.StringHelper in 'Functions\uFunctions.StringHelper.pas',
  uFunctions.InetHelper in 'Functions\uFunctions.InetHelper.pas',
  uFaceApi.ServersAccess.Types in 'Library\uFaceApi.ServersAccess.Types.pas',
  uFaceApi.Core.PersonGroup in 'Library\uFaceApi.Core.PersonGroup.pas',
  uIFaceApi.PersonGroup in 'Library\uIFaceApi.PersonGroup.pas',
  uFaceApi.Core.Face in 'Library\uFaceApi.Core.Face.pas',
  uIFaceApi.Face in 'Library\uIFaceApi.Face.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
