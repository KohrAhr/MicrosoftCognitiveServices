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
    btnVerifyTwoFacesWay1: TButton;
    Label1: TLabel;
    edtPersonGroupUserData: TEdit;
    btnDeletePersonGroup: TButton;
    edtFaceTempID1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtFaceTempID2: TEdit;
    btnVerifyTwoFacesWay2: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtPersonID: TEdit;
    edtUrl: TEdit;
    btnIdentify: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtPersonGroupName: TEdit;
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
    procedure btnVerifyTwoFacesWay1Click(Sender: TObject);
    procedure btnDeletePersonGroupClick(Sender: TObject);
    procedure btnVerifyTwoFacesWay2Click(Sender: TObject);
    procedure btnIdentifyClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  { TFaceAttributes }
  uFaceApi.FaceAttributes,
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { Detect }
  uFaceApi.FaceDetectOptions,
  { FaceApiHelper }
  uFunctions.FaceApiHelper,
  { Access }
  uFaceApi.ServersAccess.Types;

{$R *.dfm}

procedure TfmMain.btnClearLogClick(Sender: TObject);
begin
  memLog.Clear;
end;

procedure TfmMain.btnCreatePersonClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.CreateNewPerson(
      Access(edtAccessKey.Text, fasWestUS),
      edtPersonGroupID.Text, edtPersonName.Text, edtPersonUserData.Text)
  );
end;

procedure TfmMain.btnDetectInFileClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.DetectFile(
      Access(edtAccessKey.Text, fasWestUS),
      'C:\Temp\index.jpg', Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
    )
  );
end;

procedure TfmMain.btnDetectInUrlClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.DetectURL(
      Access(edtAccessKey.Text, fasWestUS),
      edtUrl.Text, Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
    )
  );
end;

procedure TfmMain.btnDetectInStreamClick(Sender: TObject);
var
  LRequestContent: TStringStream;
begin
  LRequestContent := TStringStream.Create;
  try
    LRequestContent.LoadFromFile('C:\Temp\index.jpg');

    memLog.Lines.Add(
      FaceApiHelper.DetectStream(
        Access(edtAccessKey.Text, fasWestUS),
        LRequestContent, Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
      )
    );
  finally
    LRequestContent.Free;
  end;
end;


procedure TfmMain.btnListPersonGroupsClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.ListPersonGroups(Access(edtAccessKey.Text, fasWestUS))
  );
end;

procedure TfmMain.btnListPersonsInPersonGroupClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.ListPersonsInPersonGroup(Access(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text)
  );
end;

procedure TfmMain.btnRunPersonGroupTrainingClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.TrainPersonGroup(Access(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text);

  if LResult = '' then
    LResult := 'Training for group was requested! Check your status now';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnGetPersonGroupTrainingStatusClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.GetPersonGroupTrainingStatus(Access(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text)
  );
end;

procedure TfmMain.btnCreatePersonGroupClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.CreatePersonGroup(
    Access(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text, edtPersonGroupName.Text, edtPersonGroupUserData.Text
  );

  if LResult = '' then
    LResult := 'Group was created';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnDeletePersonGroupClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.DeletePersonGroup(Access(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text);

  if LResult = '' then
    LResult := 'Group was deleted';

  memLog.Lines.Add(LResult);
end;


procedure TfmMain.btnVerifyTwoFacesWay1Click(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.Verify(
      Access(edtAccessKey.Text, fasWestUS),
      edtFaceTempID1.Text, edtFaceTempID2.Text
    )
  );
end;

procedure TfmMain.btnVerifyTwoFacesWay2Click(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.Verify(
      Access(edtAccessKey.Text, fasWestUS),
      edtFaceTempID1.Text, edtPersonID.Text, edtPersonGroupID.Text
    )
  );
end;

procedure TfmMain.btnIdentifyClick(Sender: TObject);
var
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
    if edtFaceTempID1.Text <> '' then
      LStringList.Add(edtFaceTempID1.Text);
    if edtFaceTempID2.Text <> '' then
      LStringList.Add(edtFaceTempID2.Text);

    memLog.Lines.Add(
      FaceApiHelper.Identify(
        Access(edtAccessKey.Text, fasWestUS),
        LStringList, edtPersonGroupID.Text
      )
    );
  finally
    LStringList.Free;
  end;
end;

end.
