object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Microsoft Cognitive Services demo'
  ClientHeight = 442
  ClientWidth = 768
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    768
    442)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAccessKey: TLabel
    Left = 505
    Top = 192
    Width = 53
    Height = 13
    Caption = 'Access key'
  end
  object lblPersonName: TLabel
    Left = 496
    Top = 138
    Width = 62
    Height = 13
    Caption = 'Person name'
  end
  object lblPersonUserData: TLabel
    Left = 476
    Top = 165
    Width = 82
    Height = 13
    Caption = 'Person user data'
  end
  object Label1: TLabel
    Left = 494
    Top = 111
    Width = 64
    Height = 13
    Caption = 'Person group'
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
    Width = 753
    Height = 203
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
    Top = 40
    Width = 202
    Height = 25
    Caption = 'Detect in URL'
    TabOrder = 2
    OnClick = btnDetectInUrlClick
  end
  object btnDetectInStream: TButton
    Left = 8
    Top = 72
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
  object edtPersonGroup: TEdit
    Left = 564
    Top = 108
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'edtPersonGroup'
  end
  object edtAccessKey: TEdit
    Left = 564
    Top = 189
    Width = 197
    Height = 21
    TabOrder = 7
    Text = '4acb98b9002d4d87878b54bed21af7bc'
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
    Left = 564
    Top = 135
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'edtPersonName'
  end
  object edtPersonUserData: TEdit
    Left = 564
    Top = 162
    Width = 197
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
    Top = 135
    Width = 202
    Height = 25
    Caption = 'Create Person Group'
    TabOrder = 14
    OnClick = btnCreatePersonGroupClick
  end
end
