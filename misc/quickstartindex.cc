/* quickstartindex.cc: Simplest possible indexer
 *
 * ----START-LICENCE----
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2003,2004 Olly Betts
 *
 * Modified for `Lookup Wikipedia' by kawabata.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 * -----END-LICENCE-----
 */

#include <xapian.h>
#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <algorithm>

using namespace std;

#define MAX_KEY 230

#define SPLIT_TITLE_INTO_KEYWORDS

// We will perform case insensitive searches, so we need a function to lowcase() a string
char to_lower (const char c) { return tolower(c); } 
void lowcase(string& s) { transform(s.begin(), s.end(), s.begin(), to_lower); }

int main(int argc, char **argv)
{
  // Simplest possible options parsing: we just require three or more
  // parameters.

  // Catch any Xapian::Error exceptions thrown
    
  unsigned total = 0;

  try {
	// Make the database
	Xapian::WritableDatabase database("db/", Xapian::DB_CREATE_OR_OPEN);

	string docId;
	Xapian::Document newdocument;
	int cnt=0;

	while(1) {
	  string title;
	  if (cin.eof()) break;
	  getline(cin, title);
	  int l = title.length();

	  // cout << "debug: " << title << endl;
	  // docId
	  if (l>4 && title[0] == '#' && title.substr(l-4, 4) == ".bz2") {
		docId = title.substr(1, string::npos);	
		cout << docId << " : indexing... " << endl;
		continue;
	  }

	  // Title
	  if (title[0] == '!' && title.length() > 2) {

		title=title.substr(1, string::npos);

		try {
		  // cout << "Added " << newdocument.get_data() << endl;
		  // Add the document to the database
		  database.add_document(newdocument);
		} catch(const Xapian::Error &error) {
		  cout << "Exception: "  << error.get_msg();
		  cout << "\nWhen adding:\n" << title;
		  cout << "\nOf length " << title.length() << endl;
		}

		newdocument.clear_values();
		newdocument.clear_terms();
		string Title = title;
		lowcase(title);

		// Target: filename and the exact title used
		string target = docId + string(":") + Title;
		if (target.length()>MAX_KEY)
		  target = target.substr(0, MAX_KEY);
		newdocument.set_data(target);

		// 1st Source: the lowercased title
		if (title.length() > MAX_KEY)
		  title = title.substr(0, MAX_KEY);
		newdocument.add_posting(title.c_str(), 1);
		// cout << title << " title is indexed so far" << endl;
		cnt = 2;
		continue;
	  }

	  // 2nd source: All the title's lowercased words
	  if (title.length() > MAX_KEY)
		title = title.substr(0, MAX_KEY);
	  newdocument.add_posting(title.c_str(), cnt++);
	  // cout << title << " wakati is indexed so far" << endl;
	  total ++;

	  //if ((total % 8192) == 0) {
	  //	cerr << total << " articles indexed so far" << endl;
	  //}
	}
  } catch(const Xapian::Error &error) {
	cout << "Exception: "  << error.get_msg() << endl;
  }
  cerr << total << " articles indexed." << endl;
}
