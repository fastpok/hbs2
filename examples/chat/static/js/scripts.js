function autoResize(element) {
  element.style.height = "auto";
  const scrollHeight = element.scrollHeight;
  const maxHeight = parseFloat(getComputedStyle(element).lineHeight) * 10; // 10 rows

  if (scrollHeight <= maxHeight) {
    element.style.height = scrollHeight + "px";
    element.style.overflowY = "hidden";
  } else {
    element.style.height = maxHeight + "px";
    element.style.overflowY = "auto";
  }
}

function isElementScrolledToBottom(element) {
  return (
    Math.abs(element.scrollHeight - element.scrollTop - element.clientHeight) <=
    1
  );
}

function scrollToBottom(element) {
  element.scrollTop = element.scrollHeight;
}
