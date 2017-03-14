function [time, suffix] = scale_time(seconds)
time = seconds;
suffix = 'secs';

if (time < 1)
  time = 1000*time;
  suffix = 'ms';
end

if (time < 1 )
  time = 1000*time;
  suffix = 'μs';
end

if (time < 1)
  time = 1000*time;
  suffix = 'ns';
end

end
