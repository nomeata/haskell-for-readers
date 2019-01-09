<script defer="defer">
function onload(){
  var elements = document.querySelectorAll(".Solution");
  function reveal() { this.classList.add("revealed"); }
  for (var i = 0; i < elements.length; i++) {
    elements[i].addEventListener("click", reveal);
  }
};
document.addEventListener( "DOMContentLoaded", onload, false );
</script>
