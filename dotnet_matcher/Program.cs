using System.Text.RegularExpressions;

string print_result (Match match) {
  string str = "";
  GroupCollection grp = match.Groups;
  for (int i=0; i<grp.Count; i++) {
    str += "#";
    str += i.ToString();
    str += ":";
    str +=  grp[i];
    str += "\n";
  }
  return (str);
}

// matches a regex and a string, prints the result
int matcher (string regex_str, string input_str, bool linear) {
  Regex rx = linear? new Regex(regex_str,RegexOptions.NonBacktracking) : new Regex(regex_str);

  Match match = rx.Match(input_str);
  if (match.Success) {
    Console.WriteLine (print_result(match));
  }
  else {
    Console.WriteLine ("NoMatch\n");
  }
  return 0;
}

// matches a regex and a string, prints the time it took for matching
int timer (string regex_str, string input_str, bool linear) {
  Regex rx = linear? new Regex(regex_str,RegexOptions.NonBacktracking) : new Regex(regex_str);
  var watch = System.Diagnostics.Stopwatch.StartNew();
  Match match = rx.Match(input_str);
  watch.Stop();
  float seconds = ((float) watch.ElapsedMilliseconds) / 1000;
  Console.WriteLine (seconds.ToString("N8"));
  return 0;
}

string[] arguments = Environment.GetCommandLineArgs();
string linear_str = arguments[1];
bool linear = linear_str == "linear";
string timer_str = arguments[2];
bool time = timer_str == "timer";
string regex_str = arguments[3];
string input_str = arguments[4];

if (time) {
  timer (regex_str, input_str, linear);
 }
 else {
   matcher(regex_str, input_str, linear);
 }
