summary_table = function(predmat) {
  TP = predmat[2,2]
  FP = predmat[1,2]
  TN = predmat[1,1]
  FN = predmat[2,1]
  sens = TP/(TP+FN)
  spec = TN/(TN + FP)
  ppv = TP/(FP+TP)
  npv = TN/(TN+FN)
  acc = (TP+TN)/(TP+FP+TN+FN)
  list(acc=acc,sens=sens, spec=spec, ppv=ppv, npv=npv)
}