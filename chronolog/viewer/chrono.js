function show_allresults()
{
  document.getElementById("allresults").style.display = "block";
  document.getElementById("summary").style.display = "none";
  document.getElementById("views").style.display = "none";
}

function show_summary()
{
  document.getElementById("allresults").style.display = "none";
  document.getElementById("summary").style.display = "block";
  document.getElementById("views").style.display = "none";
}

function show_customviews()
{
  document.getElementById("allresults").style.display = "none";
  document.getElementById("summary").style.display = "none";
  document.getElementById("views").style.display = "block";
}

