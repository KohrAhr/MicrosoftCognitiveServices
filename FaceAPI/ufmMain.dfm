object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Microsoft Cognitive Services demo'
  ClientHeight = 572
  ClientWidth = 848
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    848
    572)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAccessKey: TLabel
    Left = 13
    Top = 99
    Width = 53
    Height = 13
    Caption = 'Access key'
  end
  object btnDetectInFile: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Detect in file'
    TabOrder = 0
    OnClick = btnDetectInFileClick
  end
  object memLog: TMemo
    Left = 8
    Top = 160
    Width = 833
    Height = 405
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WantTabs = True
  end
  object btnDetectInUrl: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Detect in URL'
    TabOrder = 2
    OnClick = btnDetectInUrlClick
  end
  object btnDetectInStream: TButton
    Left = 170
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Detect in Stream'
    TabOrder = 3
    OnClick = btnDetectInStreamClick
  end
  object btnListPersonGroups: TButton
    Left = 352
    Top = 8
    Width = 156
    Height = 25
    Caption = 'List Person Groups'
    TabOrder = 4
    OnClick = btnListPersonGroupsClick
  end
  object btnListPersonsInPersonGroup: TButton
    Left = 8
    Top = 56
    Width = 156
    Height = 25
    Caption = 'List Persons In Person Group'
    TabOrder = 5
    OnClick = btnListPersonsInPersonGroupClick
  end
  object edtPersonGroup: TEdit
    Left = 170
    Top = 58
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'edtPersonGroup'
  end
  object edtAccessKey: TEdit
    Left = 72
    Top = 96
    Width = 219
    Height = 21
    TabOrder = 7
    Text = '4acb98b9002d4d87878b54bed21af7bc'
  end
  object btnClearLog: TButton
    Left = 8
    Top = 129
    Width = 75
    Height = 25
    Caption = 'Clear log'
    TabOrder = 8
    OnClick = btnClearLogClick
  end
end
