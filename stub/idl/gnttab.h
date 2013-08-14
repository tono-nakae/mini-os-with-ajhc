struct gnttab_setup_table {
  uint16_t dom;
  uint32_t nr_frames;
  int16_t status;
  uint64_t* frame_list;
};

