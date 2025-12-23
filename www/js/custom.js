// =============================================================================
// MultiOmicsSurvey Dashboard - Custom JavaScript
// =============================================================================

// This file can be used for custom JavaScript functionality if needed

$(document).ready(function() {
  // Add any custom JavaScript initialization here
  
  // Example: Add tooltips to elements with data-toggle="tooltip"
  $('[data-toggle="tooltip"]').tooltip();
});

// Function to handle image zoom (if needed)
function toggleImageZoom(imgElement) {
  $(imgElement).toggleClass('zoomed');
}
