double dot_offset(double u[], double v[],
                  int ou, int ov, int n, int iters)
{
  double r = 0.0;
  for (int k = 0; k < iters; k++) {
    for (int i = 0; i < n; i++) {
      r += u[i + ou] * v[i + ov];
    }
  }
  return r;
}

double dot_offset_smarter(double u[], double v[],
                          int ou, int ov, int n, int iters)
{
  double r = 0.0;
  double *uo = u + ou;
  double *vo = v + ov;
  for (int k = 0; k < iters; k++) {
    for (int i = 0; i < n; i++) {
      r += uo[i] * vo[i];
    }
  }
  return r;
}
