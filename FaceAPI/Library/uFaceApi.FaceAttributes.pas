unit uFaceApi.FaceAttributes;

interface

type
  TFaceAttribute = (doAge = 1, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion);

  TFaceAttributes = set of TFaceAttribute;

const
  CONST_FACE_ATTRIBUTES: array [TFaceAttribute] of String = ('age', 'gender', 'headPose', 'smile', 'facialHair', 'glasses', 'emotion');

implementation

end.
