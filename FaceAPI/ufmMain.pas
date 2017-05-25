unit ufmMain;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfmMain = class(TForm)
    memLog: TMemo;
    edtPersonGroupID: TEdit;
    edtAccessKey: TEdit;
    lblAccessKey: TLabel;
    btnClearLog: TButton;
    lblPersonName: TLabel;
    edtPersonName: TEdit;
    lblPersonUserData: TLabel;
    edtPersonUserData: TEdit;
    lblPersonGroupId: TLabel;
    lblPersonGroupUserData: TLabel;
    edtPersonGroupUserData: TEdit;
    edtFaceTempID1: TEdit;
    lblFaceTempId1: TLabel;
    lblFaceTempId2: TLabel;
    edtFaceTempID2: TEdit;
    lblPersonId: TLabel;
    edtPersonID: TEdit;
    lblPersonGroupName: TLabel;
    edtPersonGroupName: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    btnListPersonGroups: TButton;
    btnRunPersonGroupTraining: TButton;
    btnGetPersonGroupTrainingStatus: TButton;
    btnCreatePersonGroup: TButton;
    btnDeletePersonGroup: TButton;
    btnUpdatePersonGroup: TButton;
    btnGetPersonGroup: TButton;
    btnDetectInFile: TButton;
    btnDetectInUrl: TButton;
    btnDetectInStream: TButton;
    btnIdentify: TButton;
    Label7: TLabel;
    btnVerifyTwoFacesWay1: TButton;
    btnVerifyTwoFacesWay2: TButton;
    edtUrl: TEdit;
    TabSheet4: TTabSheet;
    btnCreatePerson: TButton;
    btnListPersonsInPersonGroup: TButton;
    lblUrl: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    btnGroup: TButton;
    Label11: TLabel;
    btnFindSimilarWay1: TButton;
    btnFindSimilarWay2: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    memFaceTempIDs: TMemo;
    Label15: TLabel;
    edtFaceListID: TEdit;
    btnDetectInFileAsync: TButton;
    btnDetectInUrlAsync: TButton;
    btnDetectInStreamAsync: TButton;
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
    procedure btnUpdatePersonGroupClick(Sender: TObject);
    procedure btnGetPersonGroupClick(Sender: TObject);
    procedure btnGroupClick(Sender: TObject);
    procedure btnFindSimilarWay1Click(Sender: TObject);
    procedure btnFindSimilarWay2Click(Sender: TObject);
    procedure btnDetectInFileAsyncClick(Sender: TObject);
    procedure btnDetectInUrlAsyncClick(Sender: TObject);
    procedure btnDetectInStreamAsyncClick(Sender: TObject);
  private
    procedure AsyncTaskCompleted(const AResult: String);
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
  { FaceApiAsyncHelper }
  uFunctions.FaceApiAsyncHelper,
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
    FaceApiHelper.CreatePerson(
      AccessServer(edtAccessKey.Text, fasWestUS),
      edtPersonGroupID.Text, edtPersonName.Text, edtPersonUserData.Text)
  );
end;


procedure TfmMain.AsyncTaskCompleted(const AResult: String);
begin
  memLog.Lines.Add(AResult);
end;

procedure TfmMain.btnDetectInFileAsyncClick(Sender: TObject);
begin
  FaceApiAsyncHelper.DetectFile(
    AccessServer(edtAccessKey.Text, fasWestUS),
    'C:\Temp\index.jpg',
    Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion]),
    AsyncTaskCompleted
  );
end;

procedure TfmMain.btnDetectInFileClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.DetectFile(
      AccessServer(edtAccessKey.Text, fasWestUS),
      'C:\Temp\index.jpg', Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
    )
  );
end;

procedure TfmMain.btnDetectInUrlAsyncClick(Sender: TObject);
begin
  FaceApiAsyncHelper.DetectURL(
    AccessServer(edtAccessKey.Text, fasWestUS),
    edtUrl.Text,
    Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion]),
    AsyncTaskCompleted
  );
end;

procedure TfmMain.btnDetectInUrlClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.DetectURL(
      AccessServer(edtAccessKey.Text, fasWestUS),
      edtUrl.Text, Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion])
    )
  );
end;

procedure TfmMain.btnDetectInStreamAsyncClick(Sender: TObject);
var
  LRequestContent: TStringStream;
begin
  LRequestContent := TStringStream.Create;
  try
    LRequestContent.LoadFromFile('C:\Temp\index.jpg');

    memLog.Lines.Add(
      FaceApiAsyncHelper.DetectStream(
        AccessServer(edtAccessKey.Text, fasWestUS),
        LRequestContent, Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion]),
        AsyncTaskCompleted
      )
    );
  finally
    LRequestContent.Free;
  end;
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
        AccessServer(edtAccessKey.Text, fasWestUS),
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
    FaceApiHelper.ListPersonGroups(AccessServer(edtAccessKey.Text, fasWestUS))
  );
end;

procedure TfmMain.btnListPersonsInPersonGroupClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.ListPersonsInPersonGroup(AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text)
  );
end;

procedure TfmMain.btnRunPersonGroupTrainingClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.TrainPersonGroup(AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text);

  if LResult = '' then
    LResult := 'Training for group was requested! Check your status now';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnGetPersonGroupTrainingStatusClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.GetPersonGroupTrainingStatus(AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text)
  );
end;

procedure TfmMain.btnGroupClick(Sender: TObject);
var
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Text := memLog.Text;

    memLog.Lines.Add(
      FaceApiHelper.Group(AccessServer(edtAccessKey.Text, fasWestUS), LStringList)
    );
  finally
    LStringList.Free;
  end;
end;

procedure TfmMain.btnCreatePersonGroupClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.CreatePersonGroup(
    AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text, edtPersonGroupName.Text, edtPersonGroupUserData.Text
  );

  if LResult = '' then
    LResult := 'Group was created';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnDeletePersonGroupClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.DeletePersonGroup(AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text);

  if LResult = '' then
    LResult := 'Group was deleted';

  memLog.Lines.Add(LResult);
end;


procedure TfmMain.btnVerifyTwoFacesWay1Click(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.Verify(
      AccessServer(edtAccessKey.Text, fasWestUS),
      edtFaceTempID1.Text, edtFaceTempID2.Text
    )
  );
end;

procedure TfmMain.btnVerifyTwoFacesWay2Click(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.Verify(
      AccessServer(edtAccessKey.Text, fasWestUS),
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
    LStringList.Text := memLog.Text;

    memLog.Lines.Add(
      FaceApiHelper.Identify(
        AccessServer(edtAccessKey.Text, fasWestUS),
        LStringList, edtPersonGroupID.Text
      )
    );
  finally
    LStringList.Free;
  end;
end;

procedure TfmMain.btnUpdatePersonGroupClick(Sender: TObject);
var
  LResult: String;
begin
  LResult := FaceApiHelper.UpdatePersonGroup(
    AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text, edtPersonGroupName.Text, edtPersonGroupUserData.Text
  );

  if LResult = '' then
    LResult := 'Group was updated';

  memLog.Lines.Add(LResult);
end;

procedure TfmMain.btnGetPersonGroupClick(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.GetPersonGroup(AccessServer(edtAccessKey.Text, fasWestUS), edtPersonGroupID.Text)
  );
end;

procedure TfmMain.btnFindSimilarWay1Click(Sender: TObject);
begin
  memLog.Lines.Add(
    FaceApiHelper.FindSimilar(AccessServer(edtAccessKey.Text, fasWestUS), edtFaceTempID1.Text, '')
  );
end;

procedure TfmMain.btnFindSimilarWay2Click(Sender: TObject);
var
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Text := memFaceTempIDs.Text;

    memLog.Lines.Add(
      FaceApiHelper.FindSimilar(AccessServer(edtAccessKey.Text, fasWestUS), edtFaceTempID1.Text, edtFaceListID.Text)
    );
  finally
    LStringList.Free;
  end;
end;

end.
