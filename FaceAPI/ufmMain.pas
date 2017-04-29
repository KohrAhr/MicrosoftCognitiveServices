unit ufmMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btnDetectInFile: TButton;
    memLog: TMemo;
    btnDetectInUrl: TButton;
    btnDetectInStream: TButton;
    btnListPersonGroups: TButton;
    btnListPersonsInPersonGroup: TButton;
    edtPersonGroupID: TEdit;
    edtAccessKey: TEdit;
    lblAccessKey: TLabel;
    btnClearLog: TButton;
    lblPersonName: TLabel;
    edtPersonName: TEdit;
    lblPersonUserData: TLabel;
    edtPersonUserData: TEdit;
    btnCreatePerson: TButton;
    lvlPersonGroupId: TLabel;
    btnRunPersonGroupTraining: TButton;
    btnGetPersonGroupTrainingStatus: TButton;
    btnCreatePersonGroup: TButton;
    btnVerifyTwoFaces: TButton;
    Label1: TLabel;
    edtPersonGroupUserData: TEdit;
    procedure btnDetectInFileClick(Sender: TObject);
    procedure btnDetectInUrlClick(Sender: TObject);
    procedure btnDetectInStreamClick(Sender: TObject);
    procedure btnListPersonGroupsClick(Sender: TObject);
    procedure btnListPersonsInPersonGroupClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnCreatePersonClick(Sender: TObject);
    procedure btnGetPersonGroupTrainingStatusClick(Sender: TObject);
    procedure btnRunPersonGroupTrainingClick(Sender: TObject);
    procedure btnCreatePersonGroupClick(Sender: TObject);
    procedure btnVerifyTwoFacesClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  { TFaceApi }
  uFaceApi,
  { TFaceAttributes }
  uFaceApi.FaceAttributes,
  { doAge }
  uFaceApi.Servers.Types,
  { Detect }
  uFaceApi.FaceDetectOptions,
  { IFaceApi }
  uIFaceApi;

{$R *.dfm}

procedure TfmMain.btnClearLogClick(Sender: TObject);
begin
  memLog.Clear;
end;

procedure TfmMain.btnCreatePersonClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.CreatePerson(edtPersonGroupID.Text, edtPersonName.Text, edtPersonUserData.Text);

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnDetectInFileClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.DetectFile('C:\Temp\index.jpg',
    Detect(True, True,
      [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion]
    )
  );

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnDetectInUrlClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.DetectURL('http://1click.lv/faceapi/sample1.jpg', Detect);

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnDetectInStreamClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;

  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LRequestContent := TStringStream.Create;
  try
    LRequestContent.LoadFromFile('C:\Temp\index.jpg');

    LResult := LIFaceApi.DetectStream(LRequestContent, Detect(True, True));

    memLog.Lines.Add(LResult);
  finally
    LRequestContent.Free;
  end;
end;


procedure TfmMain.btnListPersonGroupsClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.ListPersonGroups;

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnListPersonsInPersonGroupClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.ListPersonsInPersonGroup(edtPersonGroupID.Text);

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnRunPersonGroupTrainingClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.TrainPersonGroup(edtPersonGroupID.Text);

  if LResult = '' then
    LResult := 'Training for group was requested! Check your status now';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnGetPersonGroupTrainingStatusClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.GetPersonGroupTrainingStatus(edtPersonGroupID.Text);

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnCreatePersonGroupClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.CreatePersonGroup(edtPersonGroupID.Text, edtPersonGroupUserData.Text);

  if LResult = '' then
    LResult := 'Group was created';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnVerifyTwoFacesClick(Sender: TObject);
begin
  //
end;

end.
