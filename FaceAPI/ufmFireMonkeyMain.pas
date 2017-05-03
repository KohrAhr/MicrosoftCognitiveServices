unit ufmFireMonkeyMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation;

type
  TfmMain = class(TForm)
    btnDetectInUrl: TButton;
    memLog: TMemo;
    edtAccessKey: TEdit;
    lblAccessKey: TLabel;
    btnListPersonGroups: TButton;
    btnClearLog: TButton;
    procedure btnDetectInUrlClick(Sender: TObject);
    procedure btnListPersonGroupsClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  { fasWestUS }
  uFaceApi.Servers.Types,
  { Detect }
  uFaceApi.FaceDetectOptions,
  { FaceApiHelper }
  uFunctions.FaceApiHelper,
  { Access }
  uFaceApi.ServersAccess.Types,
  { doAge }
  uFaceApi.FaceAttributes;

{$R *.fmx}

procedure TfmMain.btnClearLogClick(Sender: TObject);
begin
  memLog.Lines.Clear;
end;

procedure TfmMain.btnDetectInUrlClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.DetectURL(
      AccessServer(edtAccessKey.Text, fasWestUS),
      'http://1click.lv/faceapi/sample1.jpg',
      Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
    )
  );
end;

procedure TfmMain.btnListPersonGroupsClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.ListPersonGroups(AccessServer(edtAccessKey.Text, fasWestUS))
  );
end;

end.
