unit ufmFireMonkeyMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
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
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  { IFaceApi }
  uIFaceApi,
  { TFaceApi }
  uFaceApi,
  { fasWestUS }
  uFaceApi.Servers.Types,
  { Detect }
  uFaceApi.FaceDetectOptions;

{$R *.fmx}

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  memLog.Lines.Clear;
end;

procedure TForm1.btnDetectInUrlClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.DetectURL('http://1click.lv/faceapi/sample1.jpg', Detect(True, True));

  memLog.Lines.Add(LResult);
end;

procedure TForm1.btnListPersonGroupsClick(Sender: TObject);
var
  LIFaceApi: IFaceApi;
  LResult: String;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(edtAccessKey.Text, fasWestUS);

  LResult := LIFaceApi.ListPersonGroups;

  memLog.Lines.Add(LResult);
end;

end.
