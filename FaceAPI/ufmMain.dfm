object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 
    'Microsoft Cognitive Services Face Api demo Y2017 by ZAM@1CLICK.L' +
    'V. Delphi DX10'
  ClientHeight = 552
  ClientWidth = 874
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 890
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    874
    552)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAccessKey: TLabel
    Left = 594
    Top = 235
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
    Left = 595
    Top = 91
    Width = 62
    Height = 13
    Caption = 'Person name'
  end
  object lblPersonUserData: TLabel
    Left = 575
    Top = 117
    Width = 82
    Height = 13
    Caption = 'Person user data'
  end
  object lblPersonGroupId: TLabel
    Left = 578
    Top = 19
    Width = 79
    Height = 13
    Caption = 'Person Group ID'
  end
  object lblPersonGroupUserData: TLabel
    Left = 543
    Top = 67
    Width = 114
    Height = 13
    Caption = 'Person Group user data'
  end
  object lblFaceTempId1: TLabel
    Left = 582
    Top = 163
    Width = 75
    Height = 13
    Caption = 'Face Temp ID 1'
  end
  object lblFaceTempId2: TLabel
    Left = 582
    Top = 187
    Width = 75
    Height = 13
    Caption = 'Face Temp ID 2'
  end
  object lblPersonId: TLabel
    Left = 610
    Top = 139
    Width = 47
    Height = 13
    Caption = 'Person ID'
  end
  object lblPersonGroupName: TLabel
    Left = 562
    Top = 43
    Width = 95
    Height = 13
    Caption = 'Person Group Name'
  end
  object lblUrl: TLabel
    Left = 638
    Top = 211
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label14: TLabel
    Left = 586
    Top = 259
    Width = 71
    Height = 13
    Caption = 'Face Temp IDs'
  end
  object Label15: TLabel
    Left = 601
    Top = 330
    Width = 56
    Height = 13
    Caption = 'Face List ID'
  end
  object memLog: TMemo
    Left = 8
    Top = 358
    Width = 857
    Height = 187
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WantTabs = True
  end
  object edtPersonGroupID: TEdit
    Left = 663
    Top = 16
    Width = 202
    Height = 21
    TabOrder = 1
    Text = 'edtPersonGroupID'
  end
  object edtAccessKey: TEdit
    Left = 663
    Top = 232
    Width = 202
    Height = 21
    TabOrder = 2
  end
  object btnClearLog: TButton
    Left = 8
    Top = 327
    Width = 75
    Height = 25
    Caption = 'Clear log'
    TabOrder = 3
    OnClick = btnClearLogClick
  end
  object edtPersonName: TEdit
    Left = 663
    Top = 88
    Width = 202
    Height = 21
    TabOrder = 4
    Text = 'edtPersonName'
  end
  object edtPersonUserData: TEdit
    Left = 663
    Top = 112
    Width = 202
    Height = 21
    Hint = '(16 KByte text)'
    TabOrder = 5
    Text = 'edtPersonUserData'
  end
  object edtPersonGroupUserData: TEdit
    Left = 663
    Top = 64
    Width = 202
    Height = 21
    Hint = '(16 KByte text)'
    TabOrder = 6
    Text = 'edtPersonGroupUserData'
  end
  object edtFaceTempID1: TEdit
    Left = 663
    Top = 160
    Width = 202
    Height = 21
    TabOrder = 7
    Text = 'edtFaceTempID1'
  end
  object edtFaceTempID2: TEdit
    Left = 663
    Top = 184
    Width = 202
    Height = 21
    TabOrder = 8
    Text = 'edtFaceTempID2'
  end
  object edtPersonID: TEdit
    Left = 663
    Top = 136
    Width = 202
    Height = 21
    TabOrder = 9
    Text = 'edtPersonID'
  end
  object edtPersonGroupName: TEdit
    Left = 663
    Top = 40
    Width = 202
    Height = 21
    TabOrder = 10
    Text = 'edtPersonGroupName'
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 509
    Height = 313
    ActivePage = TabSheet2
    TabOrder = 11
    object TabSheet1: TTabSheet
      Caption = 'Person Group'
      object Label4: TLabel
        Left = 211
        Top = 3
        Width = 250
        Height = 26
        Caption = 
          'Enter "Person Group ID", "Person Group Name" and '#13#10'optionally "P' +
          'erson Group User Data"'
      end
      object Label5: TLabel
        Left = 211
        Top = 39
        Width = 116
        Height = 13
        Caption = 'Enter "Person Group ID"'
      end
      object Label6: TLabel
        Left = 211
        Top = 72
        Width = 116
        Height = 13
        Caption = 'Enter "Person Group ID"'
      end
      object Label8: TLabel
        Left = 211
        Top = 103
        Width = 116
        Height = 13
        Caption = 'Enter "Person Group ID"'
      end
      object Label9: TLabel
        Left = 211
        Top = 165
        Width = 116
        Height = 13
        Caption = 'Enter "Person Group ID"'
      end
      object Label10: TLabel
        Left = 211
        Top = 191
        Width = 250
        Height = 26
        Caption = 
          'Enter "Person Group ID", "Person Group Name" and '#13#10'optionally "P' +
          'erson Group User Data"'
      end
      object btnListPersonGroups: TButton
        Left = 3
        Top = 129
        Width = 202
        Height = 25
        Caption = 'List Person Groups'
        TabOrder = 0
        OnClick = btnListPersonGroupsClick
      end
      object btnRunPersonGroupTraining: TButton
        Left = 3
        Top = 160
        Width = 202
        Height = 25
        Caption = 'Run Person Group training'
        TabOrder = 1
        OnClick = btnRunPersonGroupTrainingClick
      end
      object btnGetPersonGroupTrainingStatus: TButton
        Left = 3
        Top = 98
        Width = 202
        Height = 25
        Caption = 'Get Person Group training status'
        TabOrder = 2
        OnClick = btnGetPersonGroupTrainingStatusClick
      end
      object btnCreatePersonGroup: TButton
        Left = 3
        Top = 3
        Width = 202
        Height = 25
        Caption = 'Create Person Group'
        TabOrder = 3
        OnClick = btnCreatePersonGroupClick
      end
      object btnDeletePersonGroup: TButton
        Left = 3
        Top = 34
        Width = 202
        Height = 25
        Caption = 'Delete Person Group'
        TabOrder = 4
        OnClick = btnDeletePersonGroupClick
      end
      object btnUpdatePersonGroup: TButton
        Left = 3
        Top = 191
        Width = 202
        Height = 25
        Caption = 'Update Person Group'
        TabOrder = 5
        OnClick = btnUpdatePersonGroupClick
      end
      object btnGetPersonGroup: TButton
        Left = 3
        Top = 67
        Width = 202
        Height = 25
        Caption = 'Get Person Group'
        TabOrder = 6
        OnClick = btnGetPersonGroupClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Face'
      ImageIndex = 1
      object Label7: TLabel
        Left = 211
        Top = 101
        Width = 219
        Height = 13
        Caption = 'Enter "Person Group ID" and "Face Temp IDs"'
      end
      object Label1: TLabel
        Left = 211
        Top = 39
        Width = 56
        Height = 13
        Caption = 'Enter "URL"'
      end
      object Label2: TLabel
        Left = 211
        Top = 132
        Width = 219
        Height = 13
        Caption = 'Enter "Face Temp ID 1" and "Face Temp ID 2"'
      end
      object Label3: TLabel
        Left = 211
        Top = 163
        Width = 285
        Height = 13
        Caption = 'Enter "Face Temp ID 1", "Person ID" and "Person Group ID"'
      end
      object Label11: TLabel
        Left = 211
        Top = 194
        Width = 108
        Height = 13
        Caption = 'Enter "Face Temp IDs"'
      end
      object Label12: TLabel
        Left = 211
        Top = 225
        Width = 200
        Height = 13
        Caption = 'Enter "Face Temp ID 1" and "Face List ID"'
      end
      object Label13: TLabel
        Left = 211
        Top = 256
        Width = 215
        Height = 13
        Caption = 'Enter "Face Temp ID 1" and "Face Temp IDs"'
      end
      object btnDetectInFile: TButton
        Left = 3
        Top = 3
        Width = 202
        Height = 25
        Caption = 'Detect in File'
        TabOrder = 0
        OnClick = btnDetectInFileClick
      end
      object btnDetectInUrl: TButton
        Left = 3
        Top = 34
        Width = 202
        Height = 25
        Caption = 'Detect in URL'
        TabOrder = 1
        OnClick = btnDetectInUrlClick
      end
      object btnDetectInStream: TButton
        Left = 3
        Top = 65
        Width = 202
        Height = 25
        Caption = 'Detect in Stream'
        TabOrder = 2
        OnClick = btnDetectInStreamClick
      end
      object btnIdentify: TButton
        Left = 3
        Top = 96
        Width = 202
        Height = 25
        Caption = 'Identify'
        TabOrder = 3
        OnClick = btnIdentifyClick
      end
      object btnVerifyTwoFacesWay1: TButton
        Left = 3
        Top = 127
        Width = 202
        Height = 25
        Caption = 'Verify two faces - Way 1'
        TabOrder = 4
        OnClick = btnVerifyTwoFacesWay1Click
      end
      object btnVerifyTwoFacesWay2: TButton
        Left = 3
        Top = 158
        Width = 202
        Height = 25
        Caption = 'Verify two faces - Way 2'
        TabOrder = 5
        OnClick = btnVerifyTwoFacesWay2Click
      end
      object btnGroup: TButton
        Left = 3
        Top = 189
        Width = 202
        Height = 25
        Caption = 'Group'
        TabOrder = 6
        OnClick = btnGroupClick
      end
      object btnFindSimilarWay1: TButton
        Left = 3
        Top = 220
        Width = 202
        Height = 25
        Caption = 'Find Similar - Way 1'
        TabOrder = 7
        OnClick = btnFindSimilarWay1Click
      end
      object btnFindSimilarWay2: TButton
        Left = 3
        Top = 251
        Width = 202
        Height = 25
        Caption = 'Find Similar - Way 2'
        TabOrder = 8
        OnClick = btnFindSimilarWay2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Face List'
      ImageIndex = 2
    end
    object TabSheet4: TTabSheet
      Caption = 'Person'
      ImageIndex = 3
      object btnCreatePerson: TButton
        Left = 3
        Top = 3
        Width = 202
        Height = 25
        Caption = 'Create Person'
        TabOrder = 0
        OnClick = btnCreatePersonClick
      end
      object btnListPersonsInPersonGroup: TButton
        Left = 3
        Top = 34
        Width = 202
        Height = 25
        Caption = 'List Persons In Person Group'
        TabOrder = 1
        OnClick = btnListPersonsInPersonGroupClick
      end
    end
  end
  object edtUrl: TEdit
    Left = 663
    Top = 208
    Width = 202
    Height = 21
    TabOrder = 12
    Text = 'http://1click.lv/FaceApi/sample1.jpg'
  end
  object memFaceTempIDs: TMemo
    Left = 663
    Top = 256
    Width = 202
    Height = 65
    ScrollBars = ssVertical
    TabOrder = 13
    WantTabs = True
  end
  object edtFaceListID: TEdit
    Left = 663
    Top = 327
    Width = 202
    Height = 21
    TabOrder = 14
    Text = 'edtFaceListID'
  end
end
