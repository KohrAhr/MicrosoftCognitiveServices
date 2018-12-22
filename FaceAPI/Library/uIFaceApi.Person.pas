/// <summary>
///   Unit contain the Main Interface declaration for "Person" functionality for
///   Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uIFaceApi.Person;

interface

uses
  { IFaceApiBase }
  uIFaceApi.Base;

type
  /// <summary>
  ///   Main Interface for "Person" functionality for Face API Microsoft
  //    Cognitive Services 1.0
  /// </summary>
  IFaceApiPerson = interface(IFaceApiBase)
    ['{C3D3F337-682D-41C6-895C-DBE1805E4F9E}']

    /// <summary>
    ///   Create a new person in a specified person group. A newly created
    ///   person have no registered face. The number of persons has a
    ///   subscription level limit and person group level limit. Person group
    ///   level limit is 1000 for both free and paid tier subscriptions.
    ///   Subscription level limit is 1000 for free tier subscription and can be
    ///   greater in paid tier subscriptions.
    /// </summary>
    /// <param name="AGroupID">
    ///   Specifying the target person group to create the person.
    /// </param>
    /// <param name="APersonName">
    ///   Display name of the target person. The maximum length is 128.
    /// </param>
    /// <param name="APersonUserData">
    ///   (optional)
    ///   Optional fields for user-provided data attached to a person. Size
    ///   limit is 16KB.
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns a new personId created.
    ///   JSON fields in response body:
    ///   Fields	  Type	  Description
    ///   personId	String	personID of the new created person.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 404, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function CreatePerson(const AGroupID, APersonName: String; const
      APersonUserData: String = ''): String;

    /// <summary>
    ///   Delete an existing person from a person group. Persisted face images
    ///   of the person will also be deleted.
    /// </summary>
    /// <param name="AGroupID">
    ///   Specifying the person group containing the person.
    /// </param>
    /// <param name="APersonID">
    ///   The target personId to delete.
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an empty response body.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function DeletePerson(const AGroupID, APersonID: String): String;

    /// <summary>
    ///   List all persons in a person group, and retrieve person information
    ///   (including personId, name, userData and persistedFaceIds of registered
    ///   faces of the person).
    /// </summary>
    /// <param name="AGroupID">
    ///   Id of the target person group.
    /// </param>
    /// <param name="AStart">
    ///   List persons from the least personId greater than the "start".
    ///   It contains no more than 64 characters. Default is empty.
    /// </param>
    /// <param name="ATop">
    ///   The number of persons to list, ranging in [1, 1000]. Default is 1000.
    /// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of person information that belong
    ///   to the person group.
    ///   JSON fields in response body:
    ///   Fields	          Type	  Description
    ///   personId	        String	personId of the person in the person group.
    ///   name	            String	Person's display name.
    ///   userData	        String	User-provided data attached to the person.
    ///   persistedFaceIds  Array	  persistedFaceId array of registered faces of
    ///   the person.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function ListPersonsInPersonGroup(const AGroupID, AStart: String;
      ATop: Integer): String;
  end;

implementation

end.
