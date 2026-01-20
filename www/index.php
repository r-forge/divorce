
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=preg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=preg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>

<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> This is the homepage of the <b>divoRce</b> project. On this page you can find links to papers, talks, scripts, data and software related to it.</p>


<p><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />Except noted otherwise, content on this homepage and this work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.</p>


<h3>Papers:</h3>
<!--<h4>Peer-reviewed Articles:</h4>
(all open access)</br>
<p>
<ul>
<li>Sablica, L., Hornik, K., & Rusch, T. (2026). Existence and uniqueness of maximum likelihood estimation in categorical response models.</li>
<ul> 
 <li>Replication Material: <a href="divorce-reprodscript.R">R Script</a></li>
</ul>
</ul>
</p>
-->
<p>
Sablica, L., Hornik, K., & Rusch, T. (2026). Existence and uniqueness of maximum likelihood estimation in categorical response models.
</p>
<ul>
    <li>Replication Material: <a href="divorce-reprodscript.R">R Script</a></li>
</ul>


<h3>Software:</h3>
<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

The most recent build is available for Windows and Linux here: <a href="https://r-forge.r-project.org/R/?group_id=2484">divoRce Package</a>


<h3>People:</h3>
<dl>
<li><a href="http://www.wu.ac.at/statmath/en/faculty_staff/faculty/khornik">Kurt Hornik</a></li> 
<li><a href="https://www.wu.ac.at/en/competence-center-for-empirical-research-methods/team/dr-thomas-rusch">Thomas Rusch</a></li> 
<li><a href="http://www.wu.ac.at/statmath/en/faculty_staff/faculty/lukas-sablica">Lukas Sablica</a></li> 
</dl>


</body>
</html>


