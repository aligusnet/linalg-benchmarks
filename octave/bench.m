function bench(repeats, iters, desc, func, arg)

times = zeros(1, repeats);

for i = 1:repeats
  tic();
  for j = 1:iters
    func(arg);
  end
  t = toc();
  
  times(i) = t/iters;
end

[mu, mu_suffix] = scale_time(mean(times));
[sigma, sigma_suffix] = scale_time(std(times));
[m, m_suffix] = scale_time(median(times));

fprintf('%s\n', desc);
fprintf('\tmean = %f %s\n', mu, mu_suffix);
fprintf('\tmedian = %f %s\n', m, m_suffix);
fprintf('\tstandard deviation = %f %s\n', sigma, sigma_suffix);

end
