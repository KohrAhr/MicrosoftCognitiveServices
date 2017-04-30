object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 
    'Microsoft Cognitive Services Face Api demo Y2017 by ZAM@1CLICK.L' +
    'V. Delphi DX10'
  ClientHeight = 521
  ClientWidth = 1106
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 1024
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1106
    521)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAccessKey: TLabel
    Left = 527
    Top = 207
    Width = 63
    Height = 13
    Caption = 'Access key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPersonName: TLabel
    Left = 528
    Top = 138
    Width = 62
    Height = 13
    Caption = 'Person name'
  end
  object lblPersonUserData: TLabel
    Left = 508
    Top = 165
    Width = 82
    Height = 13
    Caption = 'Person user data'
  end
  object lvlPersonGroupId: TLabel
    Left = 511
    Top = 75
    Width = 79
    Height = 13
    Caption = 'Person Group ID'
  end
  object Label1: TLabel
    Left = 476
    Top = 102
    Width = 114
    Height = 13
    Caption = 'Person Group user data'
  end
  object Label2: TLabel
    Left = 792
    Top = 42
    Width = 75
    Height = 13
    Caption = 'Face Temp ID 1'
  end
  object Label3: TLabel
    Left = 792
    Top = 69
    Width = 75
    Height = 13
    Caption = 'Face Temp ID 2'
  end
  object Label4: TLabel
    Left = 792
    Top = 143
    Width = 116
    Height = 13
    Caption = 'Enter "Person Group ID"'
  end
  object Label5: TLabel
    Left = 941
    Top = 143
    Width = 112
    Height = 13
    Caption = 'Enter "Face Temp ID 1"'
  end
  object Label6: TLabel
    Left = 792
    Top = 165
    Width = 47
    Height = 13
    Caption = 'Person ID'
  end
  object btnDetectInFile: TButton
    Left = 8
    Top = 8
    Width = 202
    Height = 25
    Caption = 'Detect in File'
    TabOrder = 0
    OnClick = btnDetectInFileClick
  end
  object memLog: TMemo
    Left = 8
    Top = 232
    Width = 1091
    Height = 282
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    WantTabs = True
  end
  object btnDetectInUrl: TButton
    Left = 8
    Top = 72
    Width = 202
    Height = 25
    Caption = 'Detect in URL'
    TabOrder = 2
    OnClick = btnDetectInUrlClick
  end
  object btnDetectInStream: TButton
    Left = 8
    Top = 104
    Width = 202
    Height = 25
    Caption = 'Detect in Stream'
    TabOrder = 3
    OnClick = btnDetectInStreamClick
  end
  object btnListPersonGroups: TButton
    Left = 247
    Top = 8
    Width = 202
    Height = 25
    Caption = 'List Person Groups'
    TabOrder = 4
    OnClick = btnListPersonGroupsClick
  end
  object btnListPersonsInPersonGroup: TButton
    Left = 247
    Top = 104
    Width = 202
    Height = 25
    Caption = 'List Persons In Person Group'
    TabOrder = 5
    OnClick = btnListPersonsInPersonGroupClick
  end
  object edtPersonGroupID: TEdit
    Left = 596
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'edtPersonGroupID'
  end
  object edtAccessKey: TEdit
    Left = 596
    Top = 205
    Width = 197
    Height = 21
    TabOrder = 7
  end
  object btnClearLog: TButton
    Left = 8
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Clear log'
    TabOrder = 8
    OnClick = btnClearLogClick
  end
  object edtPersonName: TEdit
    Left = 596
    Top = 135
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'edtPersonName'
  end
  object edtPersonUserData: TEdit
    Left = 596
    Top = 162
    Width = 121
    Height = 21
    Hint = '(16 KByte text)'
    TabOrder = 10
    Text = 'edtPersonUserData'
  end
  object btnCreatePerson: TButton
    Left = 495
    Top = 8
    Width = 123
    Height = 25
    Caption = 'Create Person'
    TabOrder = 11
    OnClick = btnCreatePersonClick
  end
  object btnRunPersonGroupTraining: TButton
    Left = 247
    Top = 72
    Width = 202
    Height = 25
    Caption = 'Run Person Group training'
    TabOrder = 12
    OnClick = btnRunPersonGroupTrainingClick
  end
  object btnGetPersonGroupTrainingStatus: TButton
    Left = 247
    Top = 40
    Width = 202
    Height = 25
    Caption = 'Get Person Group training status'
    TabOrder = 13
    OnClick = btnGetPersonGroupTrainingStatusClick
  end
  object btnCreatePersonGroup: TButton
    Left = 247
    Top = 136
    Width = 202
    Height = 25
    Caption = 'Create Person Group'
    TabOrder = 14
    OnClick = btnCreatePersonGroupClick
  end
  object btnVerifyTwoFacesWay1: TButton
    Left = 792
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Verify two faces - Way 1'
    TabOrder = 15
    OnClick = btnVerifyTwoFacesWay1Click
  end
  object edtPersonGroupUserData: TEdit
    Left = 596
    Top = 99
    Width = 121
    Height = 21
    Hint = '(16 KByte text)'
    TabOrder = 16
    Text = 'edtPersonGroupUserData'
  end
  object btnDeletePersonGroup: TButton
    Left = 247
    Top = 168
    Width = 202
    Height = 25
    Caption = 'Delete Person Group'
    TabOrder = 17
    OnClick = btnDeletePersonGroupClick
  end
  object edtFaceTempID1: TEdit
    Left = 873
    Top = 39
    Width = 225
    Height = 21
    TabOrder = 18
    Text = 'edtFaceTempID1'
  end
  object edtFaceTempID2: TEdit
    Left = 873
    Top = 66
    Width = 225
    Height = 21
    TabOrder = 19
    Text = 'edtFaceTempID2'
  end
  object btnVerifyTwoFacesWay2: TButton
    Left = 792
    Top = 112
    Width = 145
    Height = 25
    Caption = 'Verify two faces - Way 2'
    TabOrder = 20
    OnClick = btnVerifyTwoFacesWay2Click
  end
  object edtPersonID: TEdit
    Left = 845
    Top = 162
    Width = 212
    Height = 21
    TabOrder = 21
    Text = 'edtPersonID'
  end
  object edtUrl: TEdit
    Left = 8
    Top = 44
    Width = 202
    Height = 21
    TabOrder = 22
    Text = 'http://1click.lv/FaceApi/sample1.jpg'
  end
end
