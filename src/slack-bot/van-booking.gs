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
// Any failure answers {"error": "message"}.  ID must be SPREADSHEET_ID.
//
// Deploying: in an Apps Script project of the account the web app is to run
// as, replace the code with this file.  The project's manifest (Project
// Settings, "Show appsscript.json") must enable the Google Sheets API advanced
// service and declare the one scope this file needs:
//
//   "dependencies": {"enabledAdvancedServices": [
//     {"userSymbol": "Sheets", "version": "v4", "serviceId": "sheets"}]},
//   "oauthScopes": ["https://www.googleapis.com/auth/spreadsheets"],
//
// The web app cannot ask for consent itself, so run any function in the
// editor and grant every permission the consent screen asks for; without it,
// every request fails with "You do not have permission to call
// SpreadsheetApp.openById".  The first deployment (Deploy > New deployment, a
// web app executing as "Me", accessible to anyone) gives the /exec URL that
// van-booking.lisp has in *APPS-SCRIPT-URL*.  Later versions go to the same
// deployment (Deploy > Manage deployments, edit, "New version"), which keeps
// that URL.

// The only spreadsheet the web app serves, van-booking.lisp's
// *SPREADSHEET-ID*.  Anyone can call the web app, whose URL is in a public
// repository, and it runs with its deployer's access to every spreadsheet.
var SPREADSHEET_ID = '1qxx5mDsrqAqGBrwLeSs0V6R_70FSNm1-ruFWxvHVgu8';

// SPREADSHEET_ID, when it is the spreadsheet the request E names.
function spreadsheetId(e) {
  if (e.parameter.spreadsheetId !== SPREADSHEET_ID) {
    throw new Error('This web app serves only the van schedule');
  }
  return SPREADSHEET_ID;
}

function doGet(e) {
  return reply(function () {
    var spreadsheet = SpreadsheetApp.openById(spreadsheetId(e));
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
      spreadsheetId(e),
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
