initialize ~= { _ =>
  sys.props("scalac.patmat.analysisBudget") = "off"
}

