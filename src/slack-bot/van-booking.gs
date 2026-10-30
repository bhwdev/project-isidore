// SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
// SPDX-License-Identifier: AGPL-3.0-or-later

// The van schedule spreadsheet's one interface to the Slack bot
// (van-booking.lisp): a web app that reads and writes the spreadsheet on the
// bot's behalf.
//
//   GET  ?action=tabs&spreadsheetId=ID
//        -> ["Aug 31-Sep 6", ...], the tab names in tab order
//   GET  ?action=values&spreadsheetId=ID&sheet=NAME
//        -> [["cell", ...], ...], every row of the tab from the first to the
//           last with content, empty rows included, as the sheet displays it
//   POST ?spreadsheetId=ID
//        {"arguments": {"range": "NAME!F4:F5", "valueInputOption": "USER_ENTERED"},
//         "body": {"values": [["BW"], ["CG"]]}}
//        -> the Sheets API's UpdateValuesResponse
//
// Any failure answers {"error": "message"}.
//
// Deploying: in the spreadsheet's Apps Script project, replace the code with
// this file and make sure the Google Sheets API advanced service is enabled
// (Services, "Sheets").  Then Deploy > Manage deployments, edit the existing
// web app deployment and pick "New version": the deployment keeps its /exec
// URL, which van-booking.lisp has in *APPS-SCRIPT-URL*.  It executes as its
// owner and is accessible to anyone, like the deployment it replaces.

function doGet(e) {
  return reply(function () {
    var spreadsheet = SpreadsheetApp.openById(e.parameter.spreadsheetId);
    switch (e.parameter.action) {
      case 'tabs':
        return spreadsheet.getSheets().map(function (sheet) {
          return sheet.getName();
        });
      case 'values':
        var sheet = spreadsheet.getSheetByName(e.parameter.sheet);
        if (!sheet) {
          throw new Error('The spreadsheet has no tab named ' + e.parameter.sheet);
        }
        return sheet.getDataRange().getDisplayValues();
      default:
        throw new Error('Unknown action ' + e.parameter.action);
    }
  });
}

function doPost(e) {
  return reply(function () {
    var request = JSON.parse(e.postData.contents);
    return Sheets.Spreadsheets.Values.update(
      request.body,
      e.parameter.spreadsheetId,
      request.arguments.range,
      {valueInputOption: request.arguments.valueInputOption});
  });
}

// The JSON of what PRODUCE returns, or of {"error": ...} when it throws.
function reply(produce) {
  var result;
  try {
    result = produce();
  } catch (error) {
    result = {error: String(error)};
  }
  return ContentService.createTextOutput(JSON.stringify(result))
    .setMimeType(ContentService.MimeType.JSON);
}
